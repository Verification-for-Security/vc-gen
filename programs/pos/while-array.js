function warray(n) {
    var i = 0;
    while (i<=n) {
        invariant (forall(j, j<0 || j>=i || arr_a[j]==1));
        arr_a[i] = 1;
        i = i + 1;
    }
    assert(forall(j, (j<0 || j>=n || arr_a[j]>=1)));
}
