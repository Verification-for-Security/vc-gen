function test(x, j) {
    assume(forall(i, a[i]>=0));
    a[i] = 2;
    assert(a[j]>=0);
}   
