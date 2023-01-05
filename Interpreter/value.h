#ifndef clox_value_h
#define clox_value_h

#include <string.h>

#include "common.h"

typedef struct Obj Obj;
typedef struct ObjString ObjString;

#ifdef NAN_BOXING

#define SIGN_BIT ((uint64_t)0x8000000000000000) //we use MSB = 1 to indicate object
#define QNAN	 ((uint64_t)0x7ffc000000000000) //quiet NaN has every exponent bit and first 2 mantissa bits set. We will use these to store data outside of numbers

#define TAG_NIL		1 //01
#define TAG_FALSE	2 //10
#define TAG_TRUE	3 //11

typedef uint64_t Value; //if NaN boxing is enabled, every value is represented as a 64 bit integer

#define AS_BOOL(value)		((value) == TRUE_VAL) //assume value is a lox boolean
#define AS_NUMBER(value)	valueToNum(value)
#define AS_OBJ(value)		((Obj*)(uintptr_t)((value) & ~(SIGN_BIT | QNAN))) //~ is bitwise not, we clear quiet NaN and sign bits and let pointer bits remain

#define IS_BOOL(value)		(((value) | 1) == TRUE_VAL)
#define IS_NIL(value)		((value) == NIL_VAL)
#define IS_FALSE(value)		((value) == FALSE_VAL)
#define IS_TRUE(value)		((value) == TRUE_VAL)
#define IS_NUMBER(value)	(((value) & QNAN) != QNAN) //every value that is not a number will use a quiet nan representation, so we & our value with all of the quiet nan bits
#define IS_OBJ(value)		(((value) & (QNAN | SIGN_BIT)) == (QNAN | SIGN_BIT)) //if sign bit and every negative bit is set, we have obj

#define BOOL_VAL(b)			((b) ? TRUE_VAL : FALSE_VAL) //if we pass in c boolean 'true', otherwise it's false
#define NIL_VAL				((Value)(uint64_t)(QNAN | TAG_NIL))
#define FALSE_VAL			((Value)(uint64_t)(QNAN | TAG_FALSE))
#define TRUE_VAL			((Value)(uint64_t)(QNAN | TAG_TRUE))
#define NUMBER_VAL(num)		numToValue(num) //representation of number is exact same in clox as a C double
#define OBJ_VAL(obj)		(Value)(SIGN_BIT | QNAN | (uint64_t)(uintptr_t)(obj)) //uintptr is unsigned integer type that can also store a pointer (can be converted back)

static inline double valueToNum(Value value) {
	double num;
	memcpy(&num, &value, sizeof(Value));
	return num;
}

static inline Value numToValue(double num) {
	Value value;
	memcpy(&value, &num, sizeof(double));
	return value;
}

#else 

typedef enum {
	VAL_BOOL,
	VAL_NIL,
	VAL_NUMBER,
	VAL_OBJ, //This type of value stores a pointer to the actual data, versus the other types which store the actual data as part of the value
} ValueType;

typedef struct {
	ValueType type;
	union {
		bool boolean;
		double number;
		Obj* obj;
	} as;
} Value;

#define IS_BOOL(value)		((value).type == VAL_BOOL)
#define IS_NIL(value)		((value).type == VAL_NIL)
#define IS_NUMBER(value)	((value).type == VAL_NUMBER)
#define IS_OBJ(value)		((value).type == VAL_OBJ)

#define AS_BOOL(value)		((value).as.boolean)
#define AS_NUMBER(value)	((value).as.number) //given a value of the right type, these return the corresponding C values (we must use the above macros to type check first)
#define AS_OBJ(value)		((value).as.obj)

#define BOOL_VAL(value)		((Value){VAL_BOOL, {.boolean = value}}) //|
#define NIL_VAL				((Value){VAL_NIL, {.number = 0}})		//|-------> these turn C values into our lox values 
#define NUMBER_VAL(value)	((Value){VAL_NUMBER, {.number = value}})//|
#define OBJ_VAL(object)		((Value){VAL_OBJ, {.obj = (Obj*)object}})

#endif

typedef struct {
	int capacity;
	int count;
	Value* values;
} ValueArray;

bool valuesEqual(Value a, Value b);
void initValueArray(ValueArray* array);
void writeValueArray(ValueArray* array, Value value);
void freeValueArray(ValueArray* array);
void printValue(Value value);

#endif
