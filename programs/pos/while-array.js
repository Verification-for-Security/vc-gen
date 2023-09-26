function warray(n) {
    var i = 0;
    while (i<=n) {
        invariant (forall(j, j<0 || j>=i || a[j]==1));
        a[i] = 1;
        i = i + 1;
    }
    assert(forall(j, (j<0 || j>=n || a[j]>=1)));
}
