#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>

int isSameLetter(char a, char b) {
    char diff = (a > b) ? a - b : b - a;
    return diff == 32;
}

int processArray(char* input, int length) {
    int outputLength = 0;
    for (int i=0; i<length; i++) {
        if (isSameLetter(input[i], input[i+1]) == 1) {
            i++;
        } else {
            input[outputLength] = input[i];
            outputLength++;
        }
    }
    return outputLength;
}

int removeFromArray(char* input, int length, char x) {
    int outLength = 0;
    for (int i=0; i<length; i++) {
        if (!(input[i] == x || input[i] == x + 32)) {
            input[outLength] = input[i];
            outLength++;
        }
    }
    return outLength;
}

int main() {
    struct stat stbuffer;
    FILE *inputFile = fopen("input", "r");
    fstat(fileno(inputFile), &stbuffer);
    int inSize = stbuffer.st_size + 1;
    char *input = malloc(inSize);

    int smallest = 1e9;
    char smallest_char;

    for (char c='A'; c<='Z'; c++) {
        fseek(inputFile, 0, 0);
        fgets(input, inSize, inputFile);

        int outLength = removeFromArray(input, inSize, c);
        int lastLength;
        do {
            lastLength = outLength;
            outLength = processArray(input, lastLength);
        } while (lastLength != outLength);
        
        outLength--;

        if (outLength < smallest) {
            smallest = outLength;
            smallest_char = c;
        }

        printf("%c: %d\n", c, outLength);
    }
    printf("Removing %c results in length %d\n", smallest_char, smallest);

    fclose(inputFile);
    free(input);
    return 0;
}
