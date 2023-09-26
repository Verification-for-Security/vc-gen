function locks(n, flag) {
    var lock = 0;
    while (n >= 1) {
        if (flag >= 1) { lock = 1 };
        if (flag >= 1) { lock = 0 };
        if (flag >= 1) { lock = 1 };
        if (flag >= 1) { lock = 0 };
        n = n - 1;
    }
    assert (lock == 0);
}
