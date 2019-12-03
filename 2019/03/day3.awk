#!/usr/bin/awk -f

function direction(raw) {
    return substr(raw, 1, 1);
}

function abs(v) {
    return v < 0 ? -v : v
}

function distance(raw) {
    return int(substr(raw, 2));
}

function separateCommands(raw) {
    return split(raw, arr, ",");
}

function changePosition(dir) {
    if (dir == "L")
        x--;
    else if (dir == "R")
        x++;
    else if (dir == "U")
        y++;
    else
        y--;
}

function calculateDistances() {
    if (grid[x,y] == "X" && x+y != 0)
        intersectionPaths[x,y] += steps;
    steps++;
}

function markCollisions() {
    if (grid[x,y] != "" && grid[x,y] != wirenum) {
        grid[x,y] = "X";
        manhattan = abs(x) + abs(y);
        if (manhattan < closestCollision && manhattan != 0)
            closestCollision = manhattan;
    }
    else
        grid[x,y] = wirenum;
}

BEGIN {
    closestCollision = 1000000000;
    shortestPath = 1000000000;
}

{
    wirenum++;
    x = 0;
    y = 0;
    steps = 0;
    len = separateCommands($1);
    for (i=1; i<=len; i++) {
        dir = direction(arr[i])
        dis = distance(arr[i])
        for (j=0; j<dis; j++) {
            # First pass
            if (wirenum <= 2)
                markCollisions();
            # Second pass
            else
                calculateDistances();
            changePosition(dir);
        }
    }
}

END {
    printf("Part 1: %d\n", closestCollision);
    asort(intersectionPaths);
    printf("Part 2: %d\n", intersectionPaths[1]);
}
