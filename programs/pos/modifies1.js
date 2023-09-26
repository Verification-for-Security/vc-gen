function f() {
    a[0] = 1;
    x = g(a);
    //havoc(a);
    assert(a[1]==2);
}

function g(a) {
    ensures(a[1]==2);
    modifies(a);
    a[1]=2;
    return 0;
}