#!/usr/bin/awk -f

function calc_fuel(mass) {
    return int(mass / 3) - 2;
}

{
    c = calc_fuel($1);
    while (c > 0) {
        s += c;
        c = calc_fuel(c);
    }
}
END {
    print s;
}
