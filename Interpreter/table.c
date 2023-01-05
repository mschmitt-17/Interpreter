#include <stdlib.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"

#define TABLE_MAX_LOAD 0.75 //we grow array when it is 75% full to prevent us from probing every bucket forever in findEntry

void initTable(Table* table) {
	table->count = 0;
	table->capacity = 0;
	table->entries = NULL;
}

void freeTable(Table* table) {
	FREE_ARRAY(Entry, table->entries, table->capacity);
	initTable(table);
}

static Entry* findEntry(Entry* entries, int capacity, ObjString* key) {
	uint32_t index = key->hash & (capacity - 1); //table will always be grown in powers of 2, this operation is the same as (% capacity)
	Entry* tombstone = NULL;

	for (;;) { //linear probing
		Entry* entry = &entries[index];
		if (entry->key == NULL) {
			if (IS_NIL(entry->value)) { //empty entry
				return tombstone != NULL ? tombstone : entry; //if we passed a tombstone already, this lets us insert into the tombstone entry instead of the later empty one
			}
			else { //found tombstone
				if (tombstone == NULL) tombstone = entry;
			}
		}
		else if (entry->key == key) { //found the key we were looking for
			return entry;
		}
		index = (index + 1) & (capacity - 1); //we loop back around when we read the end of the array 
	}
}

static void adjustCapacity(Table* table, int capacity) {
	Entry* entries = ALLOCATE(Entry, capacity); //creates bucket array with capacity entries
	for (int i = 0; i < capacity; i++) {
		entries[i].key = NULL;
		entries[i].value = NIL_VAL;
	}

	table->count = 0;
	for (int i = 0; i < table->capacity; i++) { //insert every entry from old array into new, larger array
		Entry* entry = &table->entries[i];
		if (entry->key == NULL) continue; //tombstones don't get copied over

		Entry* dest = findEntry(entries, capacity, entry->key);
		dest->key = entry->key;
		dest->value = entry->value;
		table->count++;
	}

	FREE_ARRAY(Entry, table->entries, table->capacity);
	table->entries = entries;
	table->capacity = capacity;
}

bool tableGet(Table* table, ObjString* key, Value* value) {
	if (table->count == 0) return false;

	Entry* entry = findEntry(table->entries, table->capacity, key);
	if (entry->key == NULL) return false;

	*value = entry->value; //this makes it so value points to entry->value
	return true;
}

bool tableSet(Table* table, ObjString* key, Value value) {
	if (table->count + 1 > table->capacity * TABLE_MAX_LOAD) {
		int capacity = GROW_CAPACITY(table->capacity);
		adjustCapacity(table, capacity);
	}
	
	Entry* entry = findEntry(table->entries, table->capacity, key);
	bool isNewKey = entry->key == NULL;
	if (isNewKey && IS_NIL(entry->value)) table->count++; //we don't want to increase count if we've run into a bucket with a tombstone
	entry->key = key;
	entry->value = value;
	return isNewKey;
}

bool tableDelete(Table* table, ObjString* key) {
	if (table->count == 0) return false;

	Entry* entry = findEntry(table->entries, table->capacity, key);
	if (entry->key == NULL) return false;

	entry->key = NULL;
	entry->value = BOOL_VAL(true); //this acts as our tombstone, letting us know not to stop probing when we wish to delete multiple entries
	return true;
}

void tableAddAll(Table* from, Table* to) {
	for (int i = 0; i < from->capacity; i++) {
		Entry* entry = &from->entries[i];
		if (entry->key != NULL) {
			tableSet(to, entry->key, entry->value);
		}
	}
}

ObjString* tableFindString(Table* table, const char* chars, int length, uint32_t hash) { //we pass in raw character array of key instead of ObjString
	if (table->count == 0) return NULL;
	uint32_t index = hash & (table->capacity - 1);
	for (;;) {
		Entry* entry = &table->entries[index];
		if (entry->key == NULL) { //found empty, non-tombstone entry
			if (IS_NIL(entry->value)) return NULL;
		} else if (entry->key->length == length && entry->key->hash == hash && memcmp(entry->key->chars, chars, length) == 0) { //found the string
			return entry->key;
		}
		index = (index + 1) & (table->capacity - 1);
	}
}

void tableRemoveWhite(Table* table) {
	for (int i = 0; i < table->capacity; i++) {
		Entry* entry = &table->entries[i];
		if (entry->key != NULL && !entry->key->obj.isMarked) { //if key isn't null and object isn't marked, string is unreachable and about to be deleted, so we remove it from the string hash table
			tableDelete(table, entry->key);
		}
	}
}

void markTable(Table* table) { //all keys and values are reachable
	for (int i = 0; i < table->capacity; i++) {
		Entry* entry = &table->entries[i];
		markObject((Obj*)entry->key);
		markValue(entry->value);
	}
}