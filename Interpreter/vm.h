#ifndef clox_vm_h
#define clox_vm_h

#include "object.h"
#include "table.h"
#include "value.h"

#define FRAMES_MAX 64
#define STACK_MAX (FRAMES_MAX * UINT8_COUNT)

typedef struct {
	ObjClosure* closure;
	uint8_t* ip; //when we return from a function, VM will jump here and resume
	Value* slots; //points into value stack at first slot function can use
} CallFrame; //"CallFrame represents a single ongoing function call"

typedef struct {
	CallFrame frames[FRAMES_MAX]; //array of callframe structs, treated like stack
	int frameCount;

	Value stack[STACK_MAX]; //declares array of Values of size 256
	Value* stackTop; //points 1 element past top of stack, so when stack is empty it points to 0
	Table globals; //table of global variables
	Table strings; //we now have table of strings, allowing us to use string internment to avoid checking character by character for string equality
	ObjString* initString;
	ObjUpvalue* openUpvalues;

	size_t bytesAllocated; //running total of number of bytes of managed memory VM has allocated
	size_t nextGC; //threshold that triggers next collection (goes up as bytesAllocated goes up and vice versa)
	Obj* objects; //points to head of list of objects
	int grayCount;
	int grayCapacity;
	Obj** grayStack; //dynamic array of pointers to objects
} VM;

typedef enum {
	INTERPRET_OK,
	INTERPRET_COMPILE_ERROR,
	INTERPRET_RUNTIME_ERROR
} InterpretResult;

extern VM vm; //allows us to access vm variable in object file

void initVM();
void freeVM();
InterpretResult interpret(const char* source);
void push(Value value);
Value pop();

#endif
