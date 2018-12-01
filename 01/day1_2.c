#include <stdio.h>
#include <stdlib.h>
#include "hashset/hashset.h"


// Originally, I used hashsets for this, which refused to work for me.
// I would then find out that the implementation I was using hashes based on the pointer, not the value, so I just wrote an O(n²) solution. yay.
// I hate myself for this. Could implement an ordered list to get O(log n) instead of O(n) for isIn, but who cares at this point.

#define inputLength 1024000 // adjust to length of input file

int isIn(int array[inputLength], int value) {
    for (int i=0; i<inputLength; i++) {
        if (array[i] == value) {
            return 1;
        }
    }
    return 0;
}

int main() {
    int position = 0;
    char line [10];
    int positions [inputLength] = {[0 ... 1023999] = -9999999};
    int currectLine = 0;

    while (1) {
    FILE* inputFile = fopen("input", "r");
        while (fgets(line, sizeof(line), inputFile)) {
            position += strtol(line, NULL, 10);
            if (isIn(positions, position) == 1) {
                printf("%d\n", position);
                return 0;
            }
            positions[currectLine] = position;
            currectLine++;
        }
    }

    return 0;
}

