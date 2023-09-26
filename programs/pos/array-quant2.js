function test(x, i, j) {
    assume(forall(i, a[i]>=0));
    a[i] = 2;
    assert(exists(j,a[j]>=2));
}   
