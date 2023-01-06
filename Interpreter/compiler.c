#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "compiler.h"
#include "memory.h"
#include "scanner.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

typedef struct {
	Token current;
	Token previous;
	bool hadError;
	bool panicMode; //if error has already occurred, we don't want a lot of other errors occurring as a result
} Parser;

typedef enum {
	PREC_NONE,			// lowest precedence
	PREC_ASSIGNMENT,	// =
	PREC_OR,			// or
	PREC_AND,			// and
	PREC_EQUALITY,		// == !=
	PREC_COMPARISON,	// < > <= >=
	PREC_TERM,			// + -
	PREC_FACTOR,		// * /
	PREC_UNARY,			// ! -
	PREC_CALL,			// . ()
	PREC_PRIMARY		// highest precedence
} Precedence;

typedef void (*ParseFn)(canAssign); //we have to pass this to every function since we want to pass it to setter function

typedef struct {
	ParseFn prefix; //function to compile prefix expression starting with token					-----
	ParseFn infix; //function to compile infix expression whose left operand is followed by token	 |------> these properties are the functions we wish to find from table given a token
	Precedence precedence; //precedence of infix expression that uses token as operator			-----
} ParseRule;

typedef struct {
	Token name;
	int depth; //scope depth
	bool isCaptured;
} Local;

typedef struct {
	uint8_t index; //which local slot the upvalue is capturing
	bool isLocal;
} Upvalue;

typedef enum {
	TYPE_FUNCTION,
	TYPE_INITIALIZER,
	TYPE_METHOD,
	TYPE_SCRIPT
} FunctionType;

typedef struct Compiler {
	struct Compiler* enclosing;
	ObjFunction* function;
	FunctionType type;

	Local locals[UINT8_COUNT]; //array of all locals in scope
	int localCount; //number of locals currently in scope
	Upvalue upvalues[UINT8_COUNT];
	int scopeDepth; //number of blocks surrounding code we're compiling (ex: 0 is global scope, 1 is first block, etc.)
} Compiler;

typedef struct ClassCompiler { //represents current, innermost class being compiled
	struct ClassCompiler* enclosing;
	bool hasSuperclass;
} ClassCompiler;

Parser parser;
Compiler* current = NULL;
ClassCompiler* currentClass = NULL;

static Chunk* currentChunk() {
	return &current->function->chunk; //current chunk is always the chunk we're in the middle of compiling 
}

static void errorAt(Token* token, const char* message) {
	if (parser.panicMode) return;
	parser.panicMode = true;
	fprintf(stderr, "[line %d] Error", token->line);

	if (token->type == TOKEN_EOF) {
		fprintf(stderr, " at end");
	} else if (token->type == TOKEN_ERROR) {
	} else {
		fprintf(stderr, " at '%.*s'", token->length, token->start); //prints token.length characters (token.length is passed as argument)
	}

	fprintf(stderr, ": %s\n", message);
	parser.hadError = true;
}

static void error(const char* message) {
	errorAt(&parser.previous, message);
}

static void errorAtCurrent(const char* message) {
	errorAt(&parser.current, message);
}

static void advance() {
	parser.previous = parser.current;

	for (;;) {
		parser.current = scanToken();
		if (parser.current.type != TOKEN_ERROR) break;

		errorAtCurrent(parser.current.start);
	}
}

static void consume(TokenType type, const char* message) { //validates token has expected type
	if (parser.current.type == type) {
		advance();
		return;
	}

	errorAtCurrent(message);
}

static bool check(TokenType type) { //checks if type matches current token type
	return parser.current.type == type;
}

static bool match(TokenType type) {
	if (!check(type)) return false; //if type doesn't match return false, otherwise return true and consumes token
	advance();
	return true;
}

static void emitByte(uint8_t byte) {
	writeChunk(currentChunk(), byte, parser.previous.line);
}

static void emitBytes(uint8_t byte1, uint8_t byte2) {
	emitByte(byte1);
	emitByte(byte2);
}

static void emitLoop(int loopStart) {
	emitByte(OP_LOOP);

	int offset = currentChunk()->count - loopStart + 2; //+2 is OP_LOOP operands we also want to skip over
	if (offset > UINT16_MAX) error("Loop body too large.");

	emitByte((offset >> 8) & 0xff);
	emitByte(offset & 0xff);
}

static int emitJump(uint8_t instruction) {
	emitByte(instruction); //bytecode instruction
	emitByte(0xff); //placeholder jump offset
	emitByte(0xff); //placeholder jump offset
	return currentChunk()->count - 2; //part of bytecode that we want to overwrite
}

static void emitReturn() {
	if (current->type == TYPE_INITIALIZER) {
		emitBytes(OP_GET_LOCAL, 0); //load slot 0, which contains this (instance)
	} else {
		emitByte(OP_NIL);
	}
	emitByte(OP_RETURN);
}

static uint8_t makeConstant(Value value) {
	int constant = addConstant(currentChunk(), value);
	if (constant > UINT8_MAX) {
		error("Too many constants in one chunk.");
		return 0;
	}
	return (uint8_t)constant;
}

static void emitConstant(Value value) {
	emitBytes(OP_CONSTANT, makeConstant(value));
}

static void patchJump(int offset) {
	int jump = currentChunk()->count - offset - 2; //-2 for offset itself

	if (jump > UINT16_MAX) {
		error("Too much code to jump over.");
	}

	currentChunk()->code[offset] = (jump >> 8) & 0xff; //right shift 8 (these are MSBs since they come first in offset), bitwise and
	currentChunk()->code[offset + 1] = jump & 0xff; //bitwise and
}

static void initCompiler(Compiler* compiler, FunctionType type) {
	compiler->enclosing = current; //points new function back to previous function
	compiler->function = NULL;
	compiler->type = type;
	compiler->localCount = 0;
	compiler->scopeDepth = 0;
	compiler->function = newFunction();
	current = compiler; //current is pointer to compiler
	if (type != TYPE_SCRIPT) {
		current->function->name = copyString(parser.previous.start, parser.previous.length);
	}

	Local* local = &current->locals[current->localCount++];
	local->depth = 0;
	local->isCaptured = false;
	if (type != TYPE_FUNCTION) { //set aside stack slot 0 for this, storing instance this is bound to (dpn't want to do this for functions)
		local->name.start = "this";
		local->name.length = 4;
	} else {
		local->name.start = ""; //compiler claims stack slot 0 for VM, then sets name as empty so user can't write identifier to refer to it
		local->name.length = 0;
	}
}

static ObjFunction* endCompiler() { //since compiler creates function object itself (all of code is wrapped in function), we return said function instead of a chunk
	emitReturn();
	ObjFunction* function = current->function;

#ifdef DEBUG_PRINT_CODE
	if (!parser.hadError) {
		disassembleChunk(currentChunk(), function->name != NULL ? function->name->chars : "<script>"); //implicit function we define has no name
	}
#endif
	current = current->enclosing; 
	return function;
}

static void beginScope() {
	current->scopeDepth++;
}

static void endScope() {
	current->scopeDepth--;

	while (current->localCount > 0 && current->locals[current->localCount - 1].depth > current->scopeDepth) { //look for all local variables in the same scope and pop them from stack
		if (current->locals[current->localCount - 1].isCaptured) {
			emitByte(OP_CLOSE_UPVALUE);
		} else {
			emitByte(OP_POP);
		}
		current->localCount--; //discards from array (same slot in array will be declared again if we use it again)
	}
}

static void expression();							//|
static void statement();							//|
static void declaration();							//|----->forward declarations for recursion
static ParseRule* getRule(TokenType type);			//|
static void parsePrecedence(Precedence precedence);	//|
static void namedVariable(Token name, bool canAssign);
static void variable(bool canAssign);
static Token syntheticToken(const char* text);

static uint8_t identifierConstant(Token* name) { //adds token's lexeme to chunks constant table (string is too big to store in bytecode stream, so we store its index in constant table)
	return makeConstant(OBJ_VAL(copyString(name->start, name->length)));
}

static bool identifiersEqual(Token* a, Token* b) {
	if (a->length != b->length) return false;
	return memcmp(a->start, b->start, a->length) == 0;
}

static int resolveLocal(Compiler* compiler, Token* name) {
	for (int i = compiler->localCount - 1; i >= 0; i--) { //localCount - 1 bc index starts at 0 and count starts at 1, walk array backwards to find variables in current scope first
		Local* local = &compiler->locals[i]; //note locals array will have exact same layout as stack at runtime
		if (identifiersEqual(name, &local->name)) {
			if (local->depth == -1) {
				error("Can't read local variable in its own initialzer."); //weird edge case
			}
			return i;
		}
	}
	return -1;
}

static int addUpvalue(Compiler* compiler, uint8_t index, bool isLocal) { //indexes in compiler's array match indexes where upvalues will live in ObjClosure
	int upvalueCount = compiler->function->upvalueCount;

	for (int i = 0; i < upvalueCount; i++) {
		Upvalue* upvalue = &compiler->upvalues[i];
		if (upvalue->index == index && upvalue->isLocal == isLocal) { //if slot index matches the upvalue we're trying to add...
			return i; //...we just return upvalue index, reusing it
		}
	}

	if (upvalueCount == UINT8_COUNT) {
		error("Too many closure variables in function.");
		return 0;
	}

	compiler->upvalues[upvalueCount].isLocal = isLocal; //isLocal tells us whether captured value is an actual local variable or an upvalue
	compiler->upvalues[upvalueCount].index = index; //local variable's slot index
	return compiler->function->upvalueCount++; //returns index of created upvalue in upvaluelist
}

static int resolveUpvalue(Compiler* compiler, Token* name) { //call this after failing to find a local variable in current function's scope
	if (compiler->enclosing == NULL) return -1; //if enclosig compiler is null, we're in outermost scope, so variable must be global and we return -1

	int local = resolveLocal(compiler->enclosing, name); //try to find variable in enclosing compiler, right outside the current function
	if (local != -1) {
		compiler->enclosing->locals[local].isCaptured = true;
		return addUpvalue(compiler, (uint8_t)local, true);
	}

	int upvalue = resolveUpvalue(compiler->enclosing, name); //this will recurse until we find the local variable we want or we run out of compilers to check
	if (upvalue != -1) {
		return addUpvalue(compiler, (uint8_t)upvalue, false);
	}

	return -1;
}

static void addLocal(Token name) {
	if (current->localCount == UINT8_COUNT) {
		error("Too many local variables in function.");
		return;
	}

	Local* local = &current->locals[current->localCount++];
	local->name = name;
	local->depth = -1;
	local->isCaptured = false;
}

static void declareVariable() {
	if (current->scopeDepth == 0) return;

	Token* name = &parser.previous;
	for (int i = current->localCount - 1; i >= 0; i--) {
		Local* local = &current->locals[i];
		if (local->depth != -1 && local->depth < current->scopeDepth) {
			break;
		}

		if (identifiersEqual(name, &local->name)) {
			error("Already a variable with this name in this scope.");
		}
	}
	addLocal(*name); //adds to compilers list of variables in current scope
}

static uint8_t parseVariable(const char* errorMessage) {
	consume(TOKEN_IDENTIFIER, errorMessage);

	declareVariable();
	if (current->scopeDepth > 0) return 0; //if declaration is local, we return a dummy index instead of the index we would normally return pointing to the chunk's constant table
	
	return identifierConstant(&parser.previous);
}

static void markInitialized() {
	if (current->scopeDepth == 0) return; //if fun is global variable we don't need this
	current->locals[current->localCount - 1].depth = current->scopeDepth;
}

static void defineVariable(uint8_t global) { //defining is when variable becomes available for use
	if (current->scopeDepth > 0) {
		markInitialized();
		return; //no code to create local variable at runtime, variable has already been initialized and we know where it is on the stack (the top)
	}
	
	emitBytes(OP_DEFINE_GLOBAL, global); //op define initializes variable, leaving value on stack, and global takes value on stack and stores it for later
}

static uint8_t argumentList() {
	uint8_t argCount = 0;
	if (!check(TOKEN_RIGHT_PAREN)) {
		do {
			expression();
			if (argCount > 255) {
				error("Can't have more than 255 arguments.");
			}
			argCount++;
		} while (match(TOKEN_COMMA)); //run through arguments until we hit parentheses at end
	}
	consume(TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
	return argCount;
}

static void and_(bool canAssign) {
	int endJump = emitJump(OP_JUMP_IF_FALSE); //if false, the left side (which has already been evaluated and put on stack) is the result of our expression

	emitByte(OP_POP);
	parsePrecedence(PREC_AND); //if true, the right side is the result of our expression

	patchJump(endJump);
}

static void binary(bool canAssign) {
	TokenType operatorType = parser.previous.type;
	ParseRule* rule = getRule(operatorType);
	parsePrecedence((Precedence)(rule->precedence + 1)); //compiles right operand, i.e. left and right operator will be on stack and then VM will perform binary operation and pop both

	switch (operatorType) {
		case TOKEN_BANG_EQUAL:		emitBytes(OP_EQUAL, OP_NOT); break;
		case TOKEN_EQUAL_EQUAL:		emitByte(OP_EQUAL); break;
		case TOKEN_GREATER:			emitByte(OP_GREATER); break;
		case TOKEN_GREATER_EQUAL:	emitBytes(OP_LESS, OP_NOT); break; //logically same as greater equal, we just compile it different
		case TOKEN_LESS:			emitByte(OP_LESS); break;
		case TOKEN_LESS_EQUAL:		emitBytes(OP_GREATER, OP_NOT); break;
		case TOKEN_PLUS:			emitByte(OP_ADD); break;
		case TOKEN_MINUS:			emitByte(OP_SUBTRACT); break;
		case TOKEN_STAR:			emitByte(OP_MULTIPLY); break;
		case TOKEN_SLASH:			emitByte(OP_DIVIDE); break;
		case TOKEN_POW:				emitByte(OP_POW); break;
		default: return;
	}
}

static void call(bool canAssign) {
	uint8_t argCount = argumentList(); //list returns number of arguments, with each argument expression generating code that leaves value on stack
	emitBytes(OP_CALL, argCount);
}

static void dot(bool canAssign) {
	consume(TOKEN_IDENTIFIER, "Expect property name after '.'.");
	uint8_t name = identifierConstant(&parser.previous); //load property name into constant table so name is available at runtime (for operand)

	if (canAssign && match(TOKEN_EQUAL)) { //when canAssign is true since equals is lower precedence than dot
		expression();
		emitBytes(OP_SET_PROPERTY, name);
	} else if (match(TOKEN_LEFT_PAREN)) { //method call
		uint8_t argCount = argumentList();
		emitBytes(OP_INVOKE, name); //invoke takes index of property name...
		emitByte(argCount); //and number of arguments passed to method (combines OP_GET_PROPERTY and OP_CALL)
	} else {
		emitBytes(OP_GET_PROPERTY, name);
	}
}

static void literal(bool canAssign) {
	switch (parser.previous.type) {
		case TOKEN_FALSE: emitByte(OP_FALSE); break;
		case TOKEN_NIL: emitByte(OP_NIL); break;
		case TOKEN_TRUE: emitByte(OP_TRUE); break;
		default: return;
	}
}

static void expression() {
	parsePrecedence(PREC_ASSIGNMENT);
}

static void block() {
	while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) { //keep parsing statements, declarations until we hit closing brace
		declaration();
	}
	consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

static void function(FunctionType type) {
	Compiler compiler; //new compiler for each function being compiled
	initCompiler(&compiler, type); //all bytecode is now being written to this compiler's function
	beginScope();

	consume(TOKEN_LEFT_PAREN, "Expect '(' after function name.");
	if (!check(TOKEN_RIGHT_PAREN)) {
		do {
			current->function->arity++;
			if (current->function->arity > 255) {
				errorAtCurrent("Can't have more than 255 parameters.");
			}
			uint8_t constant = parseVariable("Expect parameter name."); //since parameters are essentially local variables in outermost scope of function body
			defineVariable(constant);
		} while (match(TOKEN_COMMA));
	}
	consume(TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
	consume(TOKEN_LEFT_BRACE, "Expect '{' before function body.");
	block();

	ObjFunction* function = endCompiler(); //yields our completed function
	emitBytes(OP_CLOSURE, makeConstant(OBJ_VAL(function))); //store function as a constant in the surrounding function's constant table

	for (int i = 0; i < function->upvalueCount; i++) { //each OP_CLOSURE instruction is now followed by a series of bytes that specify upvalues ObjClosue should own
		emitByte(compiler.upvalues[i].isLocal ? 1 : 0); //if first byte is 1, we capture local variable; if first byte is 0 we capture upvalue
		emitByte(compiler.upvalues[i].index); //next byte is local slot or upvalue index
	}
}

static void method() {
	consume(TOKEN_IDENTIFIER, "Expect method name.");
	uint8_t constant = identifierConstant(&parser.previous); //method name

	FunctionType type = TYPE_METHOD; //function itself
	if (parser.previous.length == 4 && memcmp(parser.previous.start, "init", 4) == 0) {
		type = TYPE_INITIALIZER;
	}
	function(type);

	emitBytes(OP_METHOD, constant);
}

static void classDeclaration() {
	consume(TOKEN_IDENTIFIER, "Expect class name.");
	Token className = parser.previous;
	uint8_t nameConstant = identifierConstant(&parser.previous); //add class name to surrounding function's constant table
	declareVariable(); //add variable for class into scope

	emitBytes(OP_CLASS, nameConstant);
	defineVariable(nameConstant); //define variable before body so we can reference it in body

	ClassCompiler classCompiler;
	classCompiler.hasSuperclass = false;
	classCompiler.enclosing = currentClass;
	currentClass = &classCompiler;

	if (match(TOKEN_LESS)) {
		consume(TOKEN_IDENTIFIER, "Expect superclass name.");
		variable(false); //looks up superclass and pushes it onto stack

		if (identifiersEqual(&className, &parser.previous)) {
			error("A class can't inherit from itself");
		}

		beginScope(); //new scope to store super value (super value can be thought of as a variable for each class)
		addLocal(syntheticToken("super")); //no token for super
		defineVariable(0);

		namedVariable(className, false); //load subclass onto stack, so we have both our superclass and subclass on the stack now
		emitByte(OP_INHERIT);
		classCompiler.hasSuperclass = true;
	}

	namedVariable(className, false); //put class name back on top of stack
	consume(TOKEN_LEFT_BRACE, "Expect '{' before class body.");
	while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
		method();
	}
	consume(TOKEN_RIGHT_BRACE, "Expect '}' after class body.");
	emitByte(OP_POP); //get class name off of stack

	if (classCompiler.hasSuperclass) {
		endScope();
	}

	currentClass = currentClass->enclosing; //when we're done with inner class, current class becomes class that was enclosing
}

static void funDeclaration() {
	uint8_t global = parseVariable("Expect function name."); //function is bound to global variable if declared at top level, local variable if declared inside function or block
	markInitialized(); //function is marked as initialzed as soon as we compile the name, since functions can refer to themselves
	function(TYPE_FUNCTION);
	defineVariable(global);
}

static void varDeclaration() {
	uint8_t global = parseVariable("Expect variable name."); //keyword is followed by variable name

	if (match(TOKEN_EQUAL)) { //look for "=" followed by initializer expression
		expression();
	} else {
		emitByte(OP_NIL); //variable initialized to nil if we don't encounter an =
	}
	consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration.");

	defineVariable(global);
}

static void expressionStatement() {
	expression();
	consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
	emitByte(OP_POP);
}

static void forStatement() {
	beginScope(); //if var is declared in for loop, we want it to match scope of loop
	consume(TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");
	if (match(TOKEN_SEMICOLON)) {
		//no initializer
	} else if (match(TOKEN_VAR)) {
		varDeclaration();
	} else {
		expressionStatement();
	}

	int loopStart = currentChunk()->count; //# loop back here after increment
	int exitJump = -1;
	if (!match(TOKEN_SEMICOLON)) {
		expression();
		consume(TOKEN_SEMICOLON, "Expect ';' after loop condition.");

		exitJump = emitJump(OP_JUMP_IF_FALSE);//if condition is false *
		emitByte(OP_POP);
	}

	if (!match(TOKEN_RIGHT_PAREN)) {
		int bodyJump = emitJump(OP_JUMP); //unconditional &
		int incrementStart = currentChunk()->count; //! loop back from after body statement 
		expression(); //increment expression itself
		emitByte(OP_POP);
		consume(TOKEN_RIGHT_PAREN, "Expect ')' after for clauses");

		emitLoop(loopStart); //loop back to condition #
		loopStart = incrementStart;
		patchJump(bodyJump);
	}
	statement(); //& jump			
	emitLoop(loopStart); //loop back to increment !

	if (exitJump != -1) {
		patchJump(exitJump);
		emitByte(OP_POP); //* loops to here
	}

	endScope();
}

static void ifStatement() {
	consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
	expression(); //this will leave our condition value on top of the stack, which we can then use to determine whether to jump or not
	consume(TOKEN_RIGHT_PAREN, "Expect ')' after 'if'.");

	int thenJump = emitJump(OP_JUMP_IF_FALSE);
	emitByte(OP_POP); //pop truthy/falsey value off of stack
	statement(); //compile then branch

	int elseJump = emitJump(OP_JUMP); //unconditional jump after then

	patchJump(thenJump);
	emitByte(OP_POP);

	if (match(TOKEN_ELSE)) statement();
	patchJump(elseJump);
}

static void printStatement() {
	expression();
	consume(TOKEN_SEMICOLON, "Expect ';' after value.");
	emitByte(OP_PRINT);
}

static void returnStatement() {
	if (current->type == TYPE_SCRIPT) {
		error("Can't return from top-level code.");
	}
	
	if (match(TOKEN_SEMICOLON)) {
		emitReturn();
	} else {
		if (current->type == TYPE_INITIALIZER) {
			error("Can't return a value from an initializer.");
		}
		expression(); //expression will put the return value we want on the stack
		consume(TOKEN_SEMICOLON, "Expect ';' after return value.");
		emitByte(OP_RETURN);
	}
}

static void whileStatement() {
	int loopStart = currentChunk()->count; //loop back to here since we want to check conditional every time
	consume(TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
	expression();
	consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition'.");

	int exitJump = emitJump(OP_JUMP_IF_FALSE);
	emitByte(OP_POP);
	statement();
	emitLoop(loopStart);

	patchJump(exitJump);
	emitByte(OP_POP);
}

static void synchronize() {
	parser.panicMode = false;

	while (parser.current.type != TOKEN_EOF) { //skip tokens until we reach a statement boundary (noted by ; or TOKEN_EOF)
		if (parser.previous.type == TOKEN_SEMICOLON) return;
		switch (parser.current.type) {
			case TOKEN_CLASS:
			case TOKEN_FUN:
			case TOKEN_VAR:
			case TOKEN_FOR:
			case TOKEN_IF:
			case TOKEN_WHILE:
			case TOKEN_PRINT:
			case TOKEN_RETURN:
				return;
			default:
				; //nothing.
		}
		advance();
	}
}

static void declaration() {
	if (match(TOKEN_CLASS)) {
		classDeclaration();
	} else if (match(TOKEN_FUN)) {
		funDeclaration();
	} else if (match(TOKEN_VAR)) {
		varDeclaration();
	} else {
		statement();
	}
	
	if (parser.panicMode) synchronize(); //we want to exit panic mode when we finish a statement
}

static void statement() {
	if (match(TOKEN_PRINT)) {
		printStatement();
	} else if (match(TOKEN_FOR)) {
		forStatement();
	} else if (match(TOKEN_IF)) {
		ifStatement();
	} else if (match(TOKEN_RETURN)) {
		returnStatement();
	} else if (match(TOKEN_WHILE)) {
		whileStatement();
	} else if (match(TOKEN_LEFT_BRACE)) {
		beginScope();
		block();
		endScope();
	} else {
		expressionStatement();
	}
}

static void grouping(bool canAssign) {
	expression(); //compile expression between parentheses
	consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void number(bool canAssign) {
	double value = strtod(parser.previous.start, NULL); //string to double
	emitConstant(NUMBER_VAL(value));
}

static void or_(bool canAssign) {
	int elseJump = emitJump(OP_JUMP_IF_FALSE);
	int endJump = emitJump(OP_JUMP);

	patchJump(elseJump); //if falsey, we skip unconditional jump to evaluate second term in or statement
	emitByte(OP_POP);

	parsePrecedence(PREC_OR);
	patchJump(endJump); //if first value is true, we skip here bc we don't need to evaluate second value
}

static void string(bool canAssign) {
	emitConstant(OBJ_VAL(copyString(parser.previous.start + 1, parser.previous.length - 2))); //trims leading and trailing quotation marks, wraps string in object and puts it in constant table
}

static void namedVariable(Token name, bool canAssign) {
	uint8_t getOp, setOp;
	int arg = resolveLocal(current, &name);
	if (arg != -1) {
		getOp = OP_GET_LOCAL;
		setOp = OP_SET_LOCAL;
	} else if ((arg = resolveUpvalue(current, &name)) != -1) {
		getOp = OP_GET_UPVALUE;
		setOp = OP_SET_UPVALUE;
	} else {
		arg = identifierConstant(&name);
		getOp = OP_GET_GLOBAL;
		setOp = OP_SET_GLOBAL;
	}
	
	if (canAssign && match(TOKEN_EQUAL)) { //look for equals sign after identifier
		expression(); //compile assigned value
		emitBytes(setOp, (uint8_t)arg); //emit assignment instruction
	} else {
		emitBytes(getOp, (uint8_t)arg); //access variable
	}
}

static void variable(bool canAssign) { //creates named variable with parser.previous
	namedVariable(parser.previous, canAssign);
}

static Token syntheticToken(const char* text) {
	Token token;
	token.start = text;
	token.length = (int)strlen(text);
	return token;
}

static void super_(bool canAssign) {
	if (currentClass == NULL) {
		error("Can't use 'super' outside of a class.");
	} else if (!currentClass->hasSuperclass) {
		error("Can't use 'super' in a class with no superclass.");
	}

	consume(TOKEN_DOT, "Expect '.' after 'super'.");
	consume(TOKEN_IDENTIFIER, "Expect superclass method name.");
	uint8_t name = identifierConstant(&parser.previous); //take lexeme of method name and add it to constant table

	namedVariable(syntheticToken("this"), false); //look up current receiver and oush it onto stack
	if (match(TOKEN_LEFT_PAREN)) { //if we call super immediately after looking it up
		uint8_t argCount = argumentList();
		namedVariable(syntheticToken("super"), false);
		emitBytes(OP_SUPER_INVOKE, name);
		emitByte(argCount);
	} else {
		namedVariable(syntheticToken("super"), false); //look up superclass and push onto stack
		emitBytes(OP_GET_SUPER, name);
	}
}

static void this_(bool canAssign) {
	if (currentClass == NULL) {
		error("Can't use 'this' outside of a class."); //if currentClass is null, we aren't in a class
		return;
	}
	variable(false); //treat this as a local variable which gets magically initialized
}

static void unary(bool canAssign) {
	TokenType operatorType = parser.previous.type;

	parsePrecedence(PREC_UNARY); //compile operand

	switch (operatorType) { //...then emit bytecode to negate operand
		case TOKEN_BANG: emitByte(OP_NOT); break;
		case TOKEN_MINUS: emitByte(OP_NEGATE); break;
		default: return;
	}
}

ParseRule rules[] = {
	[TOKEN_LEFT_PAREN]		= {grouping, call,	 PREC_CALL},
	[TOKEN_RIGHT_PAREN]		= {NULL,	 NULL,	 PREC_NONE},
	[TOKEN_LEFT_BRACE]		= {NULL,	 NULL,	 PREC_NONE},
	[TOKEN_RIGHT_BRACE]		= {NULL,	 NULL,	 PREC_NONE},
	[TOKEN_COMMA]			= {NULL,	 NULL,	 PREC_NONE},
	[TOKEN_DOT]				= {NULL,	 dot,	 PREC_CALL},
	[TOKEN_MINUS]			= {unary,	 binary, PREC_TERM},
	[TOKEN_PLUS]			= {NULL,	 binary, PREC_TERM},
	[TOKEN_SEMICOLON]		= {NULL,	 NULL,	 PREC_NONE},
	[TOKEN_SLASH]			= {NULL,	 binary, PREC_FACTOR},
	[TOKEN_STAR]			= {NULL,	 binary, PREC_FACTOR},
	[TOKEN_POW]				= {NULL,	 binary, PREC_FACTOR},
	[TOKEN_BANG]			= {unary,	 NULL,	 PREC_NONE},
	[TOKEN_BANG_EQUAL]		= {NULL,	 binary, PREC_EQUALITY},
	[TOKEN_EQUAL]			= {NULL,	 NULL,	 PREC_NONE},
	[TOKEN_EQUAL_EQUAL]		= {NULL,	 binary, PREC_EQUALITY},
	[TOKEN_GREATER]			= {NULL,	 binary, PREC_COMPARISON},
	[TOKEN_GREATER_EQUAL]	= {NULL,	 binary, PREC_COMPARISON},
	[TOKEN_LESS]			= {NULL,	 binary, PREC_COMPARISON},
	[TOKEN_LESS_EQUAL]		= {NULL,	 binary, PREC_COMPARISON},
	[TOKEN_IDENTIFIER]		= {variable, NULL,	 PREC_NONE},
	[TOKEN_STRING]			= {string,	 NULL,	 PREC_NONE},
	[TOKEN_NUMBER]			= {number,	 NULL,	 PREC_NONE},
	[TOKEN_AND]				= {NULL,	 and_,	 PREC_AND},
	[TOKEN_CLASS]			= {NULL,	 NULL,	 PREC_NONE},
	[TOKEN_ELSE]			= {NULL,	 NULL,	 PREC_NONE},
	[TOKEN_FALSE]			= {literal,	 NULL,	 PREC_NONE},
	[TOKEN_FOR]				= {NULL,	 NULL,	 PREC_NONE},
	[TOKEN_FUN]				= {NULL,	 NULL,	 PREC_NONE},
	[TOKEN_IF]				= {NULL,	 NULL,	 PREC_NONE},
	[TOKEN_NIL]				= {literal,	 NULL,	 PREC_NONE},
	[TOKEN_OR]				= {NULL,	 or_,	 PREC_OR},
	[TOKEN_PRINT]			= {NULL,	 NULL,	 PREC_NONE},
	[TOKEN_RETURN]			= {NULL,	 NULL,	 PREC_NONE},
	[TOKEN_SUPER]			= {super_,	 NULL,	 PREC_NONE},
	[TOKEN_THIS]			= {this_,	 NULL,	 PREC_NONE},
	[TOKEN_TRUE]			= {literal,	 NULL,	 PREC_NONE},
	[TOKEN_VAR]				= {NULL,	 NULL,	 PREC_NONE},
	[TOKEN_WHILE]			= {NULL,	 NULL,	 PREC_NONE},
	[TOKEN_ERROR]			= {NULL,	 NULL,	 PREC_NONE},
	[TOKEN_EOF]				= {NULL,	 NULL,	 PREC_NONE},
};

static void parsePrecedence(Precedence precedence) { //parses any expression at given precedence level or higher
	advance();
	ParseFn prefixRule = getRule(parser.previous.type)->prefix;
	if (prefixRule == NULL) {
		error("Expect expression."); //first token will always be prefix expression
		return;
	}

	bool canAssign = precedence <= PREC_ASSIGNMENT;
	prefixRule(canAssign); //ex: for IDENTIFIER, this passes in canAssign as an argument to function variable

	while (precedence <= getRule(parser.current.type)->precedence) {
		advance();
		ParseFn infixRule = getRule(parser.previous.type)->infix;
		infixRule(canAssign);
	}

	if (canAssign && match(TOKEN_EQUAL)) error("Invalid assigment target.");
}
//above function will keep on looping until next token is too low precedence or not an infix operator

static ParseRule* getRule(TokenType type) {
	return &rules[type]; //returns rule at given index
}

ObjFunction* compile(const char* source) { //returns whether or not compilation succeeds
	initScanner(source);
	Compiler compiler;
	initCompiler(&compiler, TYPE_SCRIPT);

	parser.hadError = false;
	parser.panicMode = false;
	advance();
	while (!match(TOKEN_EOF)) {
		declaration();
	}
	ObjFunction* function = endCompiler();
	return parser.hadError ? NULL : function; //sends function object to VM, unless we have an error in which case it sends nothing
}

void markCompilerRoots() {
	Compiler* compiler = current;
	while (compiler != NULL) {
		markObject((Obj*)compiler->function); //only object compiler uses is function it's compiling to
		compiler = compiler->enclosing;
	}
}