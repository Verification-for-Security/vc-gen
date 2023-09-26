function abs(x) {
    var res = 0;
    if (x >= 1) {
        res = x;
    } else {
        res = 0-x;
    };
    assert(res >= 0);
}