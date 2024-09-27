function f() {
    arr_a[0] = 1;
    x = g(arr_a);
    //havoc(a);
    assert(arr_a[1]==2);
}

function g(arr_a) {
    ensures(arr_a[1]==2);
    modifies(arr_a);
    arr_a[1]=2;
    return 0;
}
