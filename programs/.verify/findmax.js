function find(arr_a, key, len) {
    assume(forall(k, arr_a[k]>=0));
    var index = 0;
    var max = -1;
    while (index < len) {
        if (arr_a[index] > max){max = arr_a[index];}
        index = index + 1;
    }
    assert(forall(k, k<0 || k>=len || max >= arr_a[k]));
}
