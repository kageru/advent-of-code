#!/usr/bin/awk -f

function isAscending(number) {
    digit1 = int(substr(number, 1, 1));
    digit2 = int(substr(number, 2, 1));
    digit3 = int(substr(number, 3, 1));
    digit4 = int(substr(number, 4, 1));
    digit5 = int(substr(number, 5, 1));
    digit6 = int(substr(number, 6, 1));
    return (digit1 <= digit2 && digit2 <= digit3 && digit3 <= digit4 && digit4 <= digit5 && digit5 <= digit6);
}

{
    split($1, inputs, "-");
    lower = inputs[1];
    upper = inputs[2];
    for (i=lower; i<upper; i++) {
        iString = i + "";
        # this is what I would have done, but awk regex doesn’t support back references
        #if (match(iString, "(.)\1") != 0 && isAscending(iString)) {
        if (isAscending(iString))
            print iString;
            #valid++;
    }
}

