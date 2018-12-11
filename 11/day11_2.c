#include <stdio.h>
#include <stdlib.h>

#define SERIAL 7347
#define DIMENSIONS 300

int initialFieldValue(int x, int y) {
    int id = x + 10;
    int power = id * y + SERIAL;
    power *= id;
    char itoa [10];
    int len = sprintf(itoa, "%d", power);
    if (len < 3) {
        return 0;
    }
    // manually set a nullbyte after the hundreds digit
    itoa[len-2] = 0;
    int parsed = (int) strtol(&itoa[len-3], NULL, 10);
    return parsed - 5;
}

void setFields(int fields[DIMENSIONS][DIMENSIONS]) {
    for (int i=0; i<DIMENSIONS; i++) {
        for (int j=0; j<DIMENSIONS; j++) {
            fields[i][j] = initialFieldValue(i+1, j+1);
        }
    }
}

// https://en.wikipedia.org/wiki/Summed-area_table
void setSummedArea(int sums[DIMENSIONS][DIMENSIONS], int input[DIMENSIONS][DIMENSIONS]) {
    sums[0][0] = input[0][0];
    // Topmost row
    for (int x=1; x<DIMENSIONS; x++) {
        int y = 0;
        sums[x][y] = input[x][y] + sums[x-1][y];
    }

    // Leftmost column
    for (int y=1; y<DIMENSIONS; y++) {
        int x = 0;
        sums[x][y] = input[x][y] + sums[x][y-1];
    }

    for (int x=1; x<DIMENSIONS; x++) {
        for (int y=1; y<DIMENSIONS; y++) {
            sums[x][y] = input[x][y] + sums[x][y-1] + sums[x-1][y] - sums[x-1][y-1];
        }
    }
}

int max(int a, int b) {
    return a > b ? a : b;
}

int calculateNeighborhood(int fields[DIMENSIONS][DIMENSIONS], int x, int y, int size) {
    return fields[x-1][y-1] + fields[x+size][y+size] - fields[x-1][y+size] - fields[x+size][y-1];
}

int main() {
    int fields[DIMENSIONS][DIMENSIONS];
    setFields(fields);
    int summedAreaFields[DIMENSIONS][DIMENSIONS];
    setSummedArea(summedAreaFields, fields);
    
    // Part 1
    int maxPower = -999999999;
    int maxX, maxY;
    int maxSize = 0;
    for (int x=1; x<DIMENSIONS-3; x++) {
        for (int y=1; y<DIMENSIONS-3; y++) {
            for (int size=3; size<DIMENSIONS-max(x, y); size++) {
                int moore = calculateNeighborhood(summedAreaFields, x, y, size);
                if (moore > maxPower) {
                    maxPower = moore;
                    maxX = x + 1;
                    maxY = y + 1;
                    maxSize = size + 1;
                }
            }
        }
    }
    printf("x: %d, y: %d, size: %d, power: %d\n", maxX, maxY, maxSize, maxPower);

    return 0;
}
