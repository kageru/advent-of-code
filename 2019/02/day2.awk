{
    len = split($1, arr, ",");
    arr[2] = 12;
    arr[3] = 2;
    for (i=1; i<len; i+=4) {
        cmd = int(arr[i]);
        e1 = int(arr[i+1]) + 1;
        e2 = int(arr[i+2]) + 1;
        target = int(arr[i+3]) + 1;
        if (cmd == 1)
            arr[target] = arr[e1] + arr[e2];
        else if (cmd == 2)
            arr[target] = arr[e1] * arr[e2];
        else if (cmd == 99)
            break;
        else
            print "Error, invalid command";
    }
    print arr[1]
}
