function copy(arr_a, arr_b, n) {
    assume(n>=1);
    var i = n-1;
    var j = 0;
    while (i>=0) {
        arr_a[j] = arr_b[i];
        i = i - 1;
        j = j + 1;
    }
    assert(j>=1);
    assert(arr_a[0]==arr_b[n-1] && arr_a[n-1] == arr_b[0]);
}
