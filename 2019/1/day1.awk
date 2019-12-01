#!/usr/bin/awk -f

function calc_fuel(mass) {
    return int(mass / 3) - 2;
}

{
    c = calc_fuel($1);
    p1 += c;
    while (c > 0) {
        p2 += c;
        c = calc_fuel(c);
    }
}
END {
    printf("Part 1: %s\n", p1)
    printf("Part 2: %s\n", p2)
}
