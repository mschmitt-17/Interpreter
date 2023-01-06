#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <time.h>
#include <math.h>

#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "object.h"
#include "memory.h"
#include "table.h"
#include "vm.h"


VM vm;

static void resetStack() {
	vm.stackTop = vm.stack;
	vm.frameCount = 0;
	vm.openUpvalues = NULL;
}

static void runtimeError(const char* format, ...) { //this allows us to pass in a variable number of arguments, allowing us to print detailed error messages
	va_list args;
	va_start(args, format);
	vfprintf(stderr, format, args);
	va_end(args);
	fputs("\n", stderr);

	for (int i = vm.frameCount - 1; i >= 0; i--) { //walk call stack from most recently called function to top-level function reporting line of current ip in each function
		CallFrame* frame = &vm.frames[i];
		ObjFunction* function = frame->closure->function;
		size_t instruction = frame->ip - function->chunk.code - 1;
		fprintf(stderr, "[line %d] in ", function->chunk.lines[instruction]);
		if (function->name == NULL) {
			fprintf(stderr, "script\n");
		} else {
			fprintf(stderr, "%s()\n", function->name->chars);
		}
	}
	resetStack();
}

static Value clockNative(int argCount, Value* args) {
	return NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
}

static Value sqrtNative(int argCount, Value* args) {
	if (argCount > 1) {
		runtimeError("Expected 1 argument but got %d", argCount);
	} else {
		return NUMBER_VAL(sqrt(AS_NUMBER(*args)));
	}
}

static void defineNative(const char* name, NativeFn function) {
	push(OBJ_VAL(copyString(name, (int)strlen(name)))); //name in Lox
	push(OBJ_VAL(newNative(function))); //pointer to C function
	tableSet(&vm.globals, AS_STRING(vm.stack[0]), vm.stack[1]);
	pop();
	pop();
}

void initVM() {
	resetStack();
	vm.objects = NULL;
	vm.bytesAllocated = 0;
	vm.nextGC = 1024 * 1024; //starting threshold is arbitrary

	vm.grayCount = 0;
	vm.grayCapacity = 0;
	vm.grayStack = NULL;

	initTable(&vm.globals);
	initTable(&vm.strings);

	vm.initString = NULL; // copying a string allocates memory, so we need to initialize it as NULL first to make sure GC doesn't read this before it's initialized
	vm.initString = copyString("init", 4); //this will be checked for every class, so we want it to always be present in string table

	defineNative("clock", clockNative);
	defineNative("sqrt", sqrtNative);
}

void freeVM() {
	freeTable(&vm.globals);
	freeTable(&vm.strings);
	vm.initString = NULL;
	freeObjects();
}

void push(Value value) {
	*vm.stackTop = value;
	vm.stackTop++;
}

Value pop() {
	vm.stackTop--;
	return *vm.stackTop;
}

static Value peek(int distance) {
	return vm.stackTop[-1 - distance];
}

static bool call(ObjClosure* closure, int argCount) { //initializing next CallFrame on stack
	if (argCount != closure->function->arity) {
		runtimeError("Expected %d arguments but got %d.", closure->function->arity, argCount);
		return false;
	}

	if (vm.frameCount == FRAMES_MAX) {
		runtimeError("Stack overflow.");
		return false;
	}
	
	CallFrame* frame = &vm.frames[vm.frameCount++];
	frame->closure = closure;
	frame->ip = closure->function->chunk.code;
	frame->slots = vm.stackTop - argCount - 1;
	return true;
}

static bool callValue(Value callee, int argCount) {
	if (IS_OBJ(callee)) {
		switch (OBJ_TYPE(callee)) {
			case OBJ_BOUND_METHOD: {
				ObjBoundMethod* bound = AS_BOUND_METHOD(callee);
				vm.stackTop[-argCount - 1] = bound->receiver; //puts receiver into slot 0 for the new callFrame (-argcount skips past args, -1 bc stacktop points past last used slot)
				return call(bound->method, argCount); //push a callframe for desired closure onto call stack
			}
			case OBJ_CLASS: {
				ObjClass* klass = AS_CLASS(callee);
				vm.stackTop[-argCount - 1] = OBJ_VAL(newInstance(klass)); //create new instance of called class and store on stack
				Value initializer;
				if (tableGet(&klass->methods, vm.initString, &initializer)) { //look for init method, if we find it call it (acts like constructor)
					return call(AS_CLOSURE(initializer), argCount);
				} else if (argCount != 0) {
					runtimeError("Expected 0 arguments but for %d.", argCount); //if there is no init function, there shouldn't be any args passed to class declaration
					return false;
				}
				return true;
			}
			case OBJ_CLOSURE:
				return call(AS_CLOSURE(callee), argCount);
			case OBJ_NATIVE: {
				NativeFn native = AS_NATIVE(callee);
				Value result = native(argCount, vm.stackTop - argCount); //call function
				if (vm.stackTop == vm.stack) { //if stack has just been reset (i.e. we have thrown a runtimeError which calls resetStack();) we know a native function has thrown an error, so we return false
					return false;
				} else {
					vm.stackTop -= argCount + 1; //lower stacktop
					push(result); //push result of function onto stack
					return true;
				}
			}
			default:
				break;
		}
	}
	runtimeError("Can only call functions and classes.");
	return false;
}

static bool invokeFromClass(ObjClass* klass, ObjString* name, int argCount) {
	Value method;
	if (!tableGet(&klass->methods, name, &method)) {
		runtimeError("Undefined property '%s'.", name->chars);
		return false;
	}
	return call(AS_CLOSURE(method), argCount); //push call to method onto callFrame stack
}

static bool invoke(ObjString* name, int argCount) {
	Value receiver = peek(argCount); //arguments passed are above receiver, so we look as many arguments below top to find receiver

	if (!IS_INSTANCE(receiver)) {
		runtimeError("Only instances have methods.");
		return false;
	}

	ObjInstance* instance = AS_INSTANCE(receiver);

	Value value;
	if (tableGet(&instance->fields, name, &value)) { //look up field before looking up method, if we find it store it on stack in place of receiver
		vm.stackTop[-argCount - 1] = value;
		return callValue(value, argCount);
	}

	return invokeFromClass(instance->klass, name, argCount);
}

static bool bindMethod(ObjClass* klass, ObjString* name) { //creates new bound method, binds it to receiver
	Value method;
	if (!tableGet(&klass->methods, name, &method)) {
		runtimeError("Undefined property '%s'.", name->chars);
		return false;
	}

	ObjBoundMethod* bound = newBoundMethod(peek(0), AS_CLOSURE(method)); //grab receiver from top of stack
	pop(); //pop instance from stack
	push(OBJ_VAL(bound)); //put bound method on stack
	return true;
}

static ObjUpvalue* captureUpvalue(Value* local) {
	ObjUpvalue* prevUpvalue = NULL;
	ObjUpvalue* upvalue = vm.openUpvalues;
	while (upvalue != NULL && upvalue->location > local) { //walk upvalue linked list, looking for preexisting upvalue
		prevUpvalue = upvalue;
		upvalue = upvalue->next;
	}

	if (upvalue != NULL && upvalue->location == local) { //if upvalue matches our desired one, we return the preexisting one
		return upvalue;
	}

	ObjUpvalue* createdUpvalue = newUpvalue(local);
	createdUpvalue->next = upvalue; //insert new upvalue into right spot in list (before upvalue, after prevupvalue)

	if (prevUpvalue == NULL) {
		vm.openUpvalues = createdUpvalue;
	} else {
		prevUpvalue->next = createdUpvalue;
	}
	return createdUpvalue;
}

static void closeUpvalues(Value* last) {
	while (vm.openUpvalues != NULL && vm.openUpvalues->location >= last) { //if upvalue's location points into the range of slots we're closing, we close it. Otherwise, we stop
		ObjUpvalue* upvalue = vm.openUpvalues;
		upvalue->closed = *upvalue->location; //copy variable's value into closed field of upvalue
		upvalue->location = &upvalue->closed; //update location of upvalue to address of upvalue's closed field
		vm.openUpvalues = upvalue->next;
	}
}

static void defineMethod(ObjString* name) {
	Value method = peek(0); //method is on top of stack
	ObjClass* klass = AS_CLASS(peek(1)); //class is under method
	tableSet(&klass->methods, name, method);
	pop();
}

static bool isFalsey(Value value) { //nil and false are "falsey", every other value behaves like true
	return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

static void concatenate() {
	ObjString* b = AS_STRING(peek(0)); //for GC, don't want to pop value then trigger GC bc we won't be able to find it then
	ObjString* a = AS_STRING(peek(1));
	int length = a->length + b->length;
	char* chars = ALLOCATE(char, length + 1);
	memcpy(chars, a->chars, a->length);
	memcpy(chars + a->length, b->chars, b->length);
	chars[length] = '\0';
	ObjString* result = takeString(chars, length);
	pop();
	pop();
	push(OBJ_VAL(result));
}

static InterpretResult run() { //every bytecode instruction has a stack effect that describes how instruction modifies stack; each expression leaves one value on stack, each statement has a total effect of 0
	CallFrame* frame = &vm.frames[vm.frameCount - 1]; //current topmost callframe

#define READ_BYTE() (*frame->ip++) //reads byte then increments ip
#define READ_CONSTANT() (frame->closure->function->chunk.constants.values[READ_BYTE()])
#define READ_SHORT() \
	(frame->ip += 2, \
	(uint16_t)((frame->ip[-2] << 8) | frame->ip[-1])) //builds 16 bit offset from our two 8-bit offsets ('|' is bitwise or)
#define READ_STRING() AS_STRING(READ_CONSTANT()) //reads operand from bytecode chunk, treating it as index into constant table and returns string at index
#define BINARY_OP(valueType, op) \
	do {if (!IS_NUMBER(peek(0)) || !IS_NUMBER(peek(1))) { \
		runtimeError("Operands must be numbers."); \
		return INTERPRET_RUNTIME_ERROR; \
		} \
		double b = AS_NUMBER(pop()); \
		double a = AS_NUMBER(pop()); \
		push(valueType(a op b)); \
	} while (false)

	for (;;) {
#ifdef DEBUG_TRACE_EXECUTION
		printf("       ");
		for (Value* slot = vm.stack; slot < vm.stackTop; slot++) {
			printf("[ ");
			printValue(*slot);
			printf(" ]");
		}
		printf("\n");
		disassembleInstruction(frame->closure->function->chunk, (int)(frame->ip - frame->closure->function->chunk.code)); //converts ip to relative offset from beginning of bytecode, then we can start disassembling
#endif
		uint8_t instruction;
		switch (instruction = READ_BYTE()) {
		case OP_CONSTANT: {
			Value constant = READ_CONSTANT();
			push(constant);
			break;
		}
		case OP_NIL:		push(NIL_VAL); break;
		case OP_TRUE:		push(BOOL_VAL(true)); break;
		case OP_FALSE:		push(BOOL_VAL(false)); break;
		case OP_POP: pop(); break;
		case OP_GET_LOCAL: {
			uint8_t slot = READ_BYTE(); 
			push(frame->slots[slot]); //push local's value onto top of stack, where later instructions can access it (now accesses numbered slot relative to beginning of frame)
			break;
		}
		case OP_GET_GLOBAL: {
			ObjString* name = READ_STRING(); //pull constant table index from operand
			Value value;
			if (!tableGet(&vm.globals, name, &value)) { //look up variable's value in hash table
				runtimeError("Undefined variable '%s'.", name->chars);
				return INTERPRET_RUNTIME_ERROR;
			}
			push(value);
			break;
		}
		case OP_GET_SUPER: {
			ObjString* name = READ_STRING();
			ObjClass* superclass = AS_CLASS(pop()); //pop superclass, pass to bindmethod which will

			if (!bindMethod(superclass, name)) {
				return INTERPRET_RUNTIME_ERROR;
			}
			break;
		}
		case OP_DEFINE_GLOBAL: {
			ObjString* name = READ_STRING();
			tableSet(&vm.globals, name, peek(0));
			pop();
			break;
		}
		case OP_SET_LOCAL: {
			uint8_t slot = READ_BYTE();
			frame->slots[slot] = peek(0); //takes value from top of stack and stores it in stack slot corresponding to variable
			break;
		}
		case OP_SET_GLOBAL: {
			ObjString* name = READ_STRING();
			if (tableSet(&vm.globals, name, peek(0))) {
				tableDelete(&vm.globals, name);
				runtimeError("Undefined variable '%s'.", name->chars); //variable must be defined before we set it equal to something
				return INTERPRET_RUNTIME_ERROR;
			}
			break;
		}
		case OP_GET_UPVALUE: {
			uint8_t slot = READ_BYTE();
			push(*frame->closure->upvalues[slot]->location); //asterisk dereferences location pointer to read value in location slot
			break;
		}
		case OP_SET_UPVALUE: {
			uint8_t slot = READ_BYTE();
			*frame->closure->upvalues[slot]->location = peek(0);
			break;
		}
		case OP_GET_PROPERTY: {
			if (!IS_INSTANCE(peek(0))) {
				runtimeError("Only instances have properties."); //if user tries to use dot notation on a string or other object that is not an instance
				return INTERPRET_RUNTIME_ERROR;
			}

			ObjInstance* instance = AS_INSTANCE(peek(0));
			ObjString* name = READ_STRING();

			Value value;
			if (tableGet(&instance->fields, name, &value) == true) {
				pop(); //pop instance off
				push(value); //push value we wanted to get onto stack
				break;
			}

			if (!bindMethod(instance->klass, name)) { //if not a field, we look for a method instead
				return INTERPRET_RUNTIME_ERROR;
			}
			break;
		}
		case OP_SET_PROPERTY: {
			if (!IS_INSTANCE(peek(1))) {
				runtimeError("Only instances have fields.");
				return INTERPRET_RUNTIME_ERROR;
			}

			ObjInstance* instance = AS_INSTANCE(peek(1));
			tableSet(&instance->fields, READ_STRING(), peek(0)); //sets field corresponding READ_STRING (field name string) to peek(0)
			Value value = pop();
			pop(); //remove second value from stack (instance), but keep top value
			push(value);
			break;
		}
		case OP_EQUAL: {
			Value b = pop();
			Value a = pop();
			push(BOOL_VAL(valuesEqual(a, b)));
			break;
		}
		case OP_GREATER:	BINARY_OP(BOOL_VAL, > ); break;
		case OP_LESS:		BINARY_OP(BOOL_VAL, < ); break;
		case OP_ADD: {
			if (IS_STRING(peek(0)) && IS_STRING(peek(1))) {
				concatenate();
			} else if (IS_NUMBER(peek(0)) && IS_NUMBER(peek(1))) {
				double b = AS_NUMBER(pop());
				double a = AS_NUMBER(pop());
				push(NUMBER_VAL(a + b));
			} else {
				runtimeError("Operands must be two numbers or two strings");
				return INTERPRET_RUNTIME_ERROR;
			}
			break;
		}
		case OP_SUBTRACT:	BINARY_OP(NUMBER_VAL, -); break;
		case OP_MULTIPLY:	BINARY_OP(NUMBER_VAL, *); break;
		case OP_DIVIDE:		BINARY_OP(NUMBER_VAL, /); break;
		case OP_POW: {
			if (IS_NUMBER(peek(0)) && IS_NUMBER(peek(1))) {
				double b = AS_NUMBER(pop());
				double a = AS_NUMBER(pop());
				push(NUMBER_VAL(pow(a, b)));
				break;
			}
		}
		case OP_NOT: {
			push(BOOL_VAL(isFalsey(pop())));
			break;
		}
		case OP_NEGATE: {
			if (!IS_NUMBER(peek(0))) {
				runtimeError("Operand must be a number.");
				return INTERPRET_RUNTIME_ERROR;
			}
			push(NUMBER_VAL(-AS_NUMBER(pop())));
			break;
		}
		case OP_PRINT: {
			printValue(pop()); //value we wish to print is on stack already from expression evaluation
			printf("\n");
			break;
		}
		case OP_JUMP: {
			uint16_t offset = READ_SHORT();
			frame->ip += offset;
			break;
		}
		case OP_JUMP_IF_FALSE: {
			uint16_t offset = READ_SHORT();
			if (isFalsey(peek(0))) frame->ip += offset;
			break;
		}
		case OP_LOOP: {
			uint16_t offset = READ_SHORT();
			frame->ip -= offset;
			break;
		}
		case OP_CALL: {
			int argCount = READ_BYTE(); //operand tells us number of arguments, as well as spots to count back from top of stack to reach function on stack
			if (!callValue(peek(argCount), argCount)) {
				return INTERPRET_COMPILE_ERROR;
			}
			frame = &vm.frames[vm.frameCount - 1]; //move frame back down after executing call
			break;
		}
		case OP_INVOKE: {
			ObjString* method = READ_STRING();
			int argCount = READ_BYTE();
			if (!invoke(method, argCount)) {
				return INTERPRET_RUNTIME_ERROR;
			}
			frame = &vm.frames[vm.frameCount - 1]; //new callFrame on stack if call succeeded, so we refresh copy of current frame
			break;
		}
					  //broken
		case OP_SUPER_INVOKE: {
			ObjString* method = READ_STRING();
			int argCount = READ_BYTE();
			ObjClass* superclass = AS_CLASS(pop());
			if (!invokeFromClass(superclass, method, argCount)) { //invokeFrom looks up given method on given class and tries to create call to it with argCount arity
				return INTERPRET_RUNTIME_ERROR;
			}
			frame = &vm.frames[vm.frameCount - 1];
			break;
		}
		case OP_CLOSURE: {
			ObjFunction* function = AS_FUNCTION(READ_CONSTANT()); //load compiled function from constant table
			ObjClosure* closure = newClosure(function); //wrap in closure
			push(OBJ_VAL(closure)); //push onto stack
			for (int i = 0; i < closure->upvalueCount; i++) { //iterate over each value closure expects
				uint8_t isLocal = READ_BYTE();
				uint8_t index = READ_BYTE();
				if (isLocal) {
					closure->upvalues[i] = captureUpvalue(frame->slots + index);
				} else {
					closure->upvalues[i] = frame->closure->upvalues[index]; //capture upvalue from surrounding function
				}
			}
			break;
		}
		case OP_CLOSE_UPVALUE: {
			closeUpvalues(vm.stackTop - 1);
			pop();
			break;
		}
		case OP_RETURN: {
			Value result = pop(); //pop return value off
			closeUpvalues(frame->slots); //close any parameters, local variables declared immediately inside function
			vm.frameCount--; //discard callFrame for returning function
			if (vm.frameCount == 0) {
				pop(); //if last function, our program is over, so we pop script function from stack and finish
				return INTERPRET_OK;
			}

			vm.stackTop = frame->slots; //move top of stack to beginning of returning function's stack window
			push(result); //push return value onto stack at new, lower location
			frame = &vm.frames[vm.frameCount - 1];
			break;
		}
		case OP_CLASS: {
			push(OBJ_VAL(newClass(READ_STRING()))); //bound to new variable, so put it on stack
			break;
		}
		case OP_INHERIT: {
			Value superclass = peek(1);
			if (!IS_CLASS(superclass)) {
				runtimeError("Superclass must be a class.");
				return INTERPRET_RUNTIME_ERROR;
			}
			ObjClass* subclass = AS_CLASS(peek(0));
			tableAddAll(&AS_CLASS(superclass)->methods, &subclass->methods); //all of inherited classes from superclass get copied into subclass's table
			pop();
			break;
		}
		case OP_METHOD: {
			defineMethod(READ_STRING());
			break;
		}
		}
	}
#undef READ_BYTE
#undef READ_SHORT
#undef READ_CONSTANT
#undef READ_STRING
#undef BINARY_OP
}

InterpretResult interpret(const char* source) {
	ObjFunction* function = compile(source);
	if (function == NULL) return INTERPRET_COMPILE_ERROR;

	push(OBJ_VAL(function)); //function is stored on stack in slot 0, which has been set aside already
	ObjClosure* closure = newClosure(function);
	pop();
	push(OBJ_VAL(closure));
	call(closure, 0);

	return run();
}