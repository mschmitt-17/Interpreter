#include <stdlib.h>

#include "compiler.h"
#include "memory.h"
#include "vm.h"

#ifdef DEBUG_LOG_GC
#include <stdio.h>
#include "debug.h"
#endif

#define GC_HEAP_GROW_FACTOR 2 //arbitrary

void* reallocate(void* pointer, size_t oldSize, size_t newSize) {
	vm.bytesAllocated += newSize - oldSize;
	if (newSize > oldSize) {
#ifdef DEBUG_STRESS_GC
		collectGarbage(); //we put GC here because this is the place where we'll go if we need to allocate more memory (and could be in need of unused memory)
#endif

		if (vm.bytesAllocated > vm.nextGC) {
			collectGarbage(); //for when our diagnostic flag is not enabled
		}
	}
	
	if (newSize == 0) {
		free(pointer);
		return NULL;
	}

	void* result = realloc(pointer, newSize);
	if (result == NULL) exit(1);
	return result;
}

void markObject(Obj* object) {
	if (object == NULL) return;
	if (object->isMarked) return; //keeps black objects from being turned back to gray, gray objects from being cycled through again
#ifdef DEBUG_LOG_GC
	printf("%p mark ", (void*)object);
	printValue(OBJ_VAL(object));
	printf("\n");
#endif

	object->isMarked = true;

	if (vm.grayCapacity < vm.grayCount + 1) {
		vm.grayCapacity = GROW_CAPACITY(vm.grayCapacity);
		vm.grayStack = (Obj**)realloc(vm.grayStack, sizeof(Obj*) * vm.grayCapacity);
		if (vm.grayStack == NULL) exit(1); //if we can't grow or create graystack
	}

	vm.grayStack[vm.grayCount++] = object;
}

void markValue(Value value) {
	if (IS_OBJ(value)) markObject(AS_OBJ(value)); //ensure value is actual heap object (not directly stored like numbers, booleans, nil)
}

static void markArray(ValueArray* array) {
	for (int i = 0; i < array->count; i++) {
		markValue(array->values[i]);
	}
}

static void blackenObject(Obj* object) { //black object is isMarked and not gray, so we don't change objects' state here at all
#ifdef DEBUG_LOG_GC
	printf("%p blacken ", (void*)object);
	printValue(OBJ_VAL(object));
	printf("\n");
#endif

	switch (object->type) {
		case OBJ_BOUND_METHOD: {
			ObjBoundMethod* bound = (ObjBoundMethod*)object;
			markValue(bound->receiver);
			markObject((Obj*)bound->method); //mark method closure
			break;
		}
		case OBJ_CLASS: {
			ObjClass* klass = (ObjClass*)object;
			markTable(&klass->methods);
			markObject((Obj*)klass->name); //keep class name fromm getting swept away
			break;
		}
		case OBJ_CLOSURE: {
			ObjClosure* closure = (ObjClosure*)object;
			markObject((Obj*)closure->function);
			for (int i = 0; i < closure->upvalueCount; i++) {
				markObject((Obj*)closure->upvalues[i]);
			}
			break;
		}
		case OBJ_FUNCTION: {
			ObjFunction* function = (ObjFunction*)object;
			markObject((Obj*)function->name); //reference to objstring containing function's name
			markArray(&function->chunk.constants);
			break;
		}
		case OBJ_INSTANCE: {
			ObjInstance* instance = (ObjInstance*)object;
			markObject((Obj*)instance->klass); //need to keep class this is instance of around
			markTable(&instance->fields);
			break;
		}
		case OBJ_UPVALUE:
			markValue(((ObjUpvalue*)object)->closed); //gray out reference to closed over value
		case OBJ_NATIVE: //no outgoing references so nothing to traverse
		case OBJ_STRING:
			break;
	}
}

static void freeObject(Obj* object) {
#ifdef DEBUG_LOG_GC
	printf("%p free type %d\n", (void*)object, object->type);
#endif

	switch (object->type) {
		case OBJ_BOUND_METHOD: {
			FREE(ObjBoundMethod, object);
			break;
		}
		case OBJ_CLASS: {
			ObjClass* klass = (ObjClass*)object;
			freeTable(&klass->methods);
			FREE(ObjClass, object);
			break;
		}
		case OBJ_CLOSURE: {
			ObjClosure* closure = (ObjClosure*)object;
			FREE_ARRAY(ObjUpvalue*, closure->upvalues, closure->upvalueCount); //free array of upvalue pointer closure owns
			FREE(ObjClosure, object); //we don't free the ObjFunction itself because there might be multiple closures that represent the same function.
			break;
		}
		case OBJ_FUNCTION: {
			ObjFunction* function = (ObjFunction*)object;
			freeChunk(&function->chunk);
			FREE(ObjFunction, object);
			break;
		}
		case OBJ_INSTANCE: {
			ObjInstance* instance = (ObjInstance*)object;
			freeTable(&instance->fields);
			FREE(ObjInstance, object);
			break;
		}
		case OBJ_NATIVE: {
			FREE(ObjNative, object);
			break;
		}
		case OBJ_STRING: {
			ObjString* string = (ObjString*)object;
			FREE_ARRAY(char, string->chars, string->length + 1); //free character array (chars points to character array)
			FREE(ObjString, object); //free object itself
			break;
		}
		case OBJ_UPVALUE: {
			FREE(ObjUpvalue, object);
			break;
		}
	}
}

static void markRoots() {
	for (Value* slot = vm.stack; slot < vm.stackTop; slot++) { //stuff on stack
		markValue(*slot);
	}

	for (int i = 0; i < vm.frameCount; i++) {
		markObject((Obj*)vm.frames[i].closure); //each callframe contains pointer to closure being called which is uses to access constants and upvalues, so we need these too
	}

	for (ObjUpvalue* upvalue = vm.openUpvalues; upvalue != NULL; upvalue = upvalue->next) { //open upvalue list
		markObject((Obj*)upvalue);
	}

	markTable(&vm.globals); //global variables
	markCompilerRoots(); //collection happens during any allocation meaning gc will also be running during compilation
	markObject((Obj*)vm.initString);
}

static void traceReferences() {
	while (vm.grayCount > 0) {
		Obj* object = vm.grayStack[--vm.grayCount]; //pull out gray object, traverse its references (marking any white ones gray), mark black
		blackenObject(object);
	}
}

static void sweep() {
	Obj* previous = NULL;
	Obj* object = vm.objects;
	while (object != NULL) {
		if (object->isMarked) {
			object->isMarked = false; //when next cycle begins, we want every node to be white
			previous = object;
			object = object->next;
		} else {
			Obj* unreached = object;
			object = object->next;
			if (previous != NULL) {
				previous->next = object; //keep linked list of objects working properly
			} else {
				vm.objects = object;
			}
			freeObject(unreached);
		}
	}
}

void collectGarbage() {
#ifdef DEBUG_LOG_GC
	printf("-- gc begin\n");
	size_t before = vm.bytesAllocated;
#endif

	markRoots();
	traceReferences();
	tableRemoveWhite(&vm.strings); //clear references to unreachable strings, only possible after tracing when we know which strings are unreachable
	sweep();

	vm.nextGC = vm.bytesAllocated * GC_HEAP_GROW_FACTOR;

#ifdef DEBUG_LOG_GC
	printf("-- gc end\n");
	printf("   collected %zu bytes (from %zu to %zu) next at %zu\n", before - vm.bytesAllocated, before, vm.bytesAllocated, vm.nextGC);
#endif
}

void freeObjects() {
	Obj* object = vm.objects;
	while (object != NULL) {
		Obj* next = object->next;
		freeObject(object);
		object = next;
	}
	free(vm.grayStack); //free just deallocates block of memory
}