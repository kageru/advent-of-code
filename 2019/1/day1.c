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

int part1() {
    FILE* inputFile = fopen("input", "r");
    char line [10];

    int fuel = 0;
    while (fgets(line, sizeof(line), inputFile)) {
        fuel += cost(atoi(line));
    }
    fclose(inputFile);
    
    return fuel;
}

int part2() {
    FILE* inputFile = fopen("input", "r");
    char line [10];

    int fuel = 0;
    while (fgets(line, sizeof(line), inputFile)) {
        fuel += costRec(atoi(line), 0);
    }
    fclose(inputFile);
    
    return fuel;
}

int main(int argc, char *argv[]) {

    printf("Part 1: %d\n", part1());
    printf("Part 2: %d\n", part2());
    return 0;
}
