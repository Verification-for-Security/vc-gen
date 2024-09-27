function f() {
    arr_a[0] = 1;
    x = g(arr_a);
    assert(arr_a[0]==1);
}

function g(arr_a) {
    modifies(arr_a);
    return 0;
}
