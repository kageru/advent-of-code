#include <stdio.h>
#define SIZE 1000

struct Claim {
    int left;
    int top;
    int width;
    int height;
    int id;
};

struct Claim parseClaim(char line[25]) {
    struct Claim claim;
    sscanf(line, "#%d @ %d,%d: %dx%d", &claim.id, &claim.left, &claim.top, &claim.width, &claim.height);
    return claim;
}

void resetFields(int fabric [SIZE][SIZE]) {
    for (int i=0; i<SIZE; i++) {
        for (int j=0; j<SIZE; j++) {
            fabric[i][j] = 0;
        }
    }
}

void setFields(struct Claim claim, int fabric [SIZE][SIZE]) {
    for (int i=claim.left; i<claim.left+claim.width; i++) {
        for (int j=claim.top; j<claim.top+claim.height; j++) {
            fabric[i][j] += 1;
        }
    }
}

int countDoubles(int fabric [SIZE][SIZE]) {
    int sum = 0;
    for (int i=0; i<SIZE; i++) {
        for (int j=0; j<SIZE; j++) {
            if (fabric[i][j] > 1) {
                sum++;
            }
        }
    }
    return sum;
}

int main() {
    int fabric [SIZE][SIZE];
    FILE* input = fopen("input", "r");
    char line [25];
    
    resetFields(fabric);
    while (fgets(line, sizeof(line), input)) {
        struct Claim claim = parseClaim(line);
        setFields(claim, fabric);
    }

    printf("%d\n", countDoubles(fabric));
    return 0;
}

