#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>

#define GENERATIONS 20
#define INSIZE 150

typedef struct GrowingArray {
    char *content;
    int size;
    int used;
    int bufferLeft;
    int bufferRight;
} Array;

Array initArray() {
    Array array;
    array.size = 10;
    array.used = 0;
    array.content = malloc(10);
    return array;
}

/*
void pushInto(Array array, char element) {
    if (array.size == array.used) {
        array.size *= 2;
        array.content = realloc(array.content, array.size);
    }
}
*/

void appendTo(Array array, char element) {
    array.content[array.used] = element;
    array.used++;
}

// Convert a growth rule (5 chars) to a single byte by creating a bitmask.
// I. e. #..#. -> 10010
char ruleToChar(char *input) {
    int currentStep = 0b10000000;
    int rule = 0;
    for (int i=0; i<5; i++) {
        if (input[i] == '#') {
            rule += currentStep;
        }
        currentStep >>= 1;
    }
    if (input[9] == '#') {
        rule |= 1;
    }
    return rule;
}

int main() {
    FILE *inputFile = fopen("input", "r");

    printf("hi");

    // + 1 for the null byte
    char *inString = malloc(INSIZE + 1);
    fgets(inString, INSIZE, inputFile);
    
    inString += 15;

    char *rawRule = malloc(12);
    // discard this as it’s just an empty line
    fgets(rawRule, 11, inputFile);

    Array rules = initArray();
    printf("hi2");
    while (fgets(rawRule, 11, inputFile)) {
        appendTo(rules, ruleToChar(rawRule));
    }

    free(inString - 15);
    fclose(inputFile);

    printf("%d, %c", rules.size, rules.content[0]);

    // Theoretically, a plant up to 2 plants away from the current edge can grow.
    // As this can happen in either direction, we need to be prepared
    // for 4 steps of growth per generation.
    char *input = malloc(INSIZE + GENERATIONS * 4);

    // Move the start pointer of the array where pot #0 is
    input += GENERATIONS * 2;


    return 0;
}
