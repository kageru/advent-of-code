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

int main() {
    struct stat stbuffer;
    FILE *inputFile = fopen("input", "r");
    fstat(fileno(inputFile), &stbuffer);
    int inSize = stbuffer.st_size + 1;
    //inSize = 20;
    //fseek(inputFile, 10020, 0);
    char *input = malloc(inSize);
    fgets(input, inSize, inputFile);

    int lastLength = inSize;
    
    int outLength = inSize;
    do {
        lastLength = outLength;
        outLength = processArray(input, lastLength);
    } while (lastLength != outLength);

    printf("%d\n", outLength - 1);

    fclose(inputFile);
    free(input);
    return 0;
}
