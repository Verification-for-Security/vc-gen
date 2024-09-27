function test(x, i, j) {
    assume(forall(i, arr_a[i]>=0));
    arr_a[i] = 2;
    assert(exists(j,arr_a[j]>=2));
}   
