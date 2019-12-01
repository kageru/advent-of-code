#!/usr/bin/awk -f

{
    s += int($1 / 3) - 2;
}
END {
    print s;
}
