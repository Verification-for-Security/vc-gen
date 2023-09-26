function copy(n) {
    assume(n>=1);
    var i = n-1;
    var j = 0;
    while (i>=0) {
        a[j] = b[i];
        i = i - 1;
        j = j + 1;
    }
    assert(j>=1);
    assert(a[0]==b[n-1] && a[n-1] == b[0]);
}
