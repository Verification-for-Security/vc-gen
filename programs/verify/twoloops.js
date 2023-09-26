function twoloops(n) {
    var i = 0;
    while (i <= 3) {
        i = i + 1;
    }
    while (i <= 7) {
        i = i + 1;
    }
    assert (i == 8);

    // {I ^ b} s {I}
    // {i <= 7} i = i + 1 {i <= 7}
    // i = 7
}
