#include <stdio.h>
#include <stdlib.h>

// Originally, I used hashsets for this, which refused to work for me.
// I would then find out that the implementation I was using hashes based on the pointer, not the value, so I just wrote a stupid lookup table.

#define inputLength 1000000 // adjust accordingly

int main() {
    int position = 0;
    char line [10];
    char positions [inputLength]; // all initialized to 0

    while (1) {
    FILE* inputFile = fopen("input", "r");
        while (fgets(line, sizeof(line), inputFile)) {
            position += strtol(line, NULL, 10);
            // Because of part 1, we know that the numbers are steadily increasing.
            // Negative numbers are therefore skipped, as we know the repeated number to be positive.
            if (position > 0 && positions[position] == 1) {
                printf("%d\n", position);
                return 0;
            }
            positions[position] = 1;
        }
    }
}

