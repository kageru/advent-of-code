#!/usr/bin/awk -f

# There are no scopes, so we just write to `arr` here and use it later.
# Returning the array does not seem to be legal either. Only scalars allowed.
function prepareInput(raw) {
    len = split(raw, arr, ",");
    for (i=0; i<=len; i++) {
        arr[i-1] = int(arr[i]);
    }
    return len;
}

function process(arr, len, first, second) {
    arr[1] = first
    arr[2] = second
    for (i=0; i<len; i+=4) {
        cmd = arr[i];
        e1 = arr[i+1];
        e2 = arr[i+2];
        target = arr[i+3];
        if (cmd == 1)
            arr[target] = arr[e1] + arr[e2];
        else if (cmd == 2)
            arr[target] = arr[e1] * arr[e2];
        else if (cmd == 99)
            break;
        else
            print "Error, invalid command";
    }
}

{
    len = prepareInput($1);
    process(arr, len, 12, 2);
    printf("Part 1: %d\n", arr[0]);
    part2_target = 19690720
    for (x=0; x<100; x++) {
        for (y=0; y<100; y++) {
            len = prepareInput($1);
            process(arr, len, x, y);
            if (arr[0] == part2_target) {
                printf("Part 2: %d\n", 100 * x + y);
            }
        }
    }
}
