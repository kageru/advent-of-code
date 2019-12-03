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

BEGIN {
    closestCollision = 1000000000;
}

{
    wirenum++;
    x = 0;
    y = 0;
    len = separateCommands($1);
    for (i=1; i<=len; i++) {
        dir = direction(arr[i])
        dis = distance(arr[i])
        for (j=0; j<dis; j++) {
            if (grid[x,y] != "" && grid[x,y] != wirenum) {
                grid[x,y] = "X";
                printf("Collision at %d,%d\n", x, y);
                manhattan = abs(x) + abs(y);
                if (manhattan < closestCollision && manhattan != 0)
                    closestCollision = manhattan;
            }
            else
                grid[x,y] = wirenum;
            changePosition(dir);
        }
    }
}

END {
    printf("Closest collision at %d\n", closestCollision);
}
