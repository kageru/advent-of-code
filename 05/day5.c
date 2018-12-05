#include <stdio.h>
#include <stdlib.h>

struct ArrayWithLength {
    int length;
    char* content;
};

int isSameLetter(char a, char b) {
    char diff = (a > b) ? a - b : b - a;
    return diff == 32;
}

struct ArrayWithLength processArray(char* input, int length) {
    char output [length];
    int outputLength = 0;
    for (int i=0; i<length-1; i++) {
        if (isSameLetter(input[i], input[i+1]) == 1) {
            i++;
        } else {
            output[outputLength] = input[i];
            outputLength++;
        }
    }
    struct ArrayWithLength ret;
    ret.length = outputLength;
    ret.content = output;
    return ret;
}

int main() {
    return 0;
}
