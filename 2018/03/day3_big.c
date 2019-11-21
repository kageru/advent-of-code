#include <stdio.h>
#define SIZE 50000

struct Claim {
    int left;
    int top;
    int width;
    int height;
    int id;
};

struct Claim parseClaim(char line[50]) {
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

int verifyClaim(struct Claim claim, int fabric [SIZE][SIZE]) {
    for (int i=claim.left; i<claim.left+claim.width; i++) {
        for (int j=claim.top; j<claim.top+claim.height; j++) {
            if (fabric[i][j] != 1) {
                return 0;
            }
        }
    }
    return 1;
}

int main() {
    int fabric [SIZE][SIZE];
    FILE* input = fopen("big_input", "r");
    char line [50];
    printf("setup done");
    
    // Looks like int[][] isn’t initialized to 0
    resetFields(fabric);
    printf("all zero");
    int i = 0;
    while (fgets(line, sizeof(line), input)) {
        struct Claim claim = parseClaim(line);
        setFields(claim, fabric);
    }
    printf("claims set");

    input = fopen("big_input", "r");
    while (fgets(line, sizeof(line), input)) {
        struct Claim claim = parseClaim(line);
        if (verifyClaim(claim, fabric) == 1) {
            printf("Valid claim: %d\n", claim.id);
            return 0;
        }
    }

    return 0;
}

