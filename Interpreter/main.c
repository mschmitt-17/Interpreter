#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "common.h"
#include "chunk.h"
#include "debug.h"
#include "vm.h"

static void repl() {
	char line[1024];
	for (;;) {
		printf("> ");
		
		if (!fgets(line, sizeof(line), stdin)) {
			printf("\n");
			break;
		}

		interpret(line);
	}
}

static char* readFile(const char* path) {
	FILE* file = fopen(path, "rb");
	
	if (file == NULL) {
		fprintf(stderr, "Could not open file \"%s\".\n", path);
		exit(74);
	}

	fseek(file, 0L, SEEK_END); //puts us at end of file
	size_t fileSize = ftell(file); //tells us how many bytes we are from start (length since we're at end)
	rewind(file); //back to beginning of file...

	char* buffer = (char*)malloc(fileSize + 1);
	
	if (buffer == NULL) {
		fprintf(stderr, "Not enough memory to read \"%s\".\n", path);
		exit(74);
	}

	size_t bytesRead = fread(buffer, sizeof(char), fileSize, file); //read whole file

	if (bytesRead < fileSize) {
		fprintf(stderr, "Could not read file\"%s\".\n", path);
	}

	buffer[bytesRead] = '\0'; //indicates end of file when we're creating tokens

	fclose(file);
	return buffer;
}

static void runFile(const char* path) {
	char* source = readFile(path);
	InterpretResult result = interpret(source);
	free(source);

	if (result == INTERPRET_COMPILE_ERROR) exit(65);
	if (result == INTERPRET_RUNTIME_ERROR) exit(70);
}

int main(int argc, const char* argv[]) {
	initVM();
	if (argc == 1) {
		repl();
		//runFile("TextFile1.txt");
	} else if (argc == 2) {
		runFile(argv[1]);
	} else {
		fprintf(stderr, "Usage: clox [path]\n");
		exit(64);
	}

	freeVM();
	return 0;
}