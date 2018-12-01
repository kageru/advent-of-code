#include <stdio.h>
#include <stdlib.h>

int main() {
    int position = 0;
    FILE* inputFile = fopen("input", "r");
    char line [10];

    while (fgets(line, sizeof(line), inputFile)) {
        position += strtol(line, NULL, 10);
    }

    printf("%d", position);

    return 0;
}

