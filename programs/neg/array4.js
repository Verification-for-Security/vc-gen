function inc(x,j,i) {
    arr_a[i] = x
    arr_b[i] = arr_a[i]+1;
    arr_b[j] = 3; 
    assert(arr_b[i]==arr_a[i]+1);
}
