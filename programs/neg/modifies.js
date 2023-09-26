function f() {
    //modifies(a);
    a[0] = 1;
    x = g(a);
    //havoc(a);
    assert(a[0]==1);
}

function g(a) {
    modifies(a);
    return 0;
}