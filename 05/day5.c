#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>

int isSameLetter(char a, char b) {
    char diff = (a > b) ? a - b : b - a;
    return diff == 32;
}

int processArray(char* input, int length) {
    int outputLength = 0;
    for (int i=0; i<length-1; i++) {
        if (isSameLetter(input[i], input[i+1]) == 1) {
            printf("reacting: %c%c\n", input[i], input[i+1]);
            i++;
        } else {
            input[outputLength] = input[i];
            outputLength++;
        }
    }
    return outputLength + 1;
}

int main() {
    struct stat stbuffer;
    FILE *inputFile = fopen("input", "r");
    fstat(fileno(inputFile), &stbuffer) != -1;
    int inSize = stbuffer.st_size;
    //inSize = 40;
    char *input = malloc(inSize);
    fgets(input, inSize, inputFile);

    int lastLength = inSize;
    
    printf("%s\n", input);
    int outLength = inSize;
    do {
        lastLength = outLength;
        outLength = processArray(input, lastLength);
        printf("%s\n", input);
        printf("%d\n", outLength);
    } while (lastLength != outLength);

    printf("%d\n", outLength);

    fclose(inputFile);
    free(input);
    return 0;
}
