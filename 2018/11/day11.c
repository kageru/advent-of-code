#include <stdio.h>
#include <stdlib.h>

#define SERIAL 7347
#define DIMENSIONS 300

/*
Find the fuel cell's rack ID, which is its X coordinate plus 10.
Begin with a power level of the rack ID times the Y coordinate.
Increase the power level by the value of the grid serial number (your puzzle input).
Set the power level to itself multiplied by the rack ID.
Keep only the hundreds digit of the power level (so 12345 becomes 3; numbers with no hundreds digit become 0).
Subtract 5 from the power level. 
 */
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

int calculateMooreNeighborhood(int fields[DIMENSIONS][DIMENSIONS], int x, int y) {
    return fields[x-2][y-2] + fields[x+1][y+1] - fields[x-2][y+1] - fields[x+1][y-2];
}

int main() {
    int fields[DIMENSIONS][DIMENSIONS];
    setFields(fields);
    int summedAreaFields[DIMENSIONS][DIMENSIONS];
    setSummedArea(summedAreaFields, fields);
    
    // Part 1
    int maxPower = -999999999;
    int maxX, maxY;
    for (int x=2; x<DIMENSIONS-1; x++) {
        for (int y=2; y<DIMENSIONS-1; y++) {
            int moore = calculateMooreNeighborhood(summedAreaFields, x, y);
            if (moore > maxPower) {
                maxPower = moore;
                maxX = x;
                maxY = y;
            }
        }
    }
    printf("x: %d, y: %d, power: %d\n", maxX, maxY, maxPower);

    return 0;
}
