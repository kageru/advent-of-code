{
    len = split($1, arr, ",");
    for (i=0; i<=len; i++) {
        arr[i-1] = arr[i];
    }
    arr[1] = 12;
    arr[2] = 2;
    for (i=0; i<len; i+=4) {
        cmd = int(arr[i]);
        e1 = int(arr[i+1]);
        e2 = int(arr[i+2]);
        target = int(arr[i+3]);
        if (cmd == 1)
            arr[target] = arr[e1] + arr[e2];
        else if (cmd == 2)
            arr[target] = arr[e1] * arr[e2];
        else if (cmd == 99)
            break;
        else
            print "Error, invalid command";
    }
    print arr[0]
}
