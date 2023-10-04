function sum(n) {
    assume (n >= 1);
    var r = 0;
    var i = n;
    while (i >= 1) {
        r = r + 1;
        i = i - 1;
    }
    assert (i == 0);
    assert (n == r);
    assert (r >= 1);
}
