#include <stdio.h>
#include <stdlib.h>

int cost(int mass) {
    return mass / 3 - 2;
}

int costRec(int mass, int acc) {
    int c = cost(mass);
    if (c <= 0) {
        return acc;
    }
    return costRec(c, acc+c);
}

int main(int argc, char *argv[]) {
    FILE* inputFile = fopen("input", "r");
    char line [10];

    int fuel = 0;
    int fuelRec = 0;
    while (fgets(line, sizeof(line), inputFile)) {
        fuel += cost(atoi(line));
        fuelRec += costRec(atoi(line), 0);
    }
    fclose(inputFile);
    
    printf("Part 1: %d\n", fuel);
    printf("Part 2: %d\n", fuelRec);
}
