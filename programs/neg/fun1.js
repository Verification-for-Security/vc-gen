function f(x) {
    requires(x>=0);
    ensures($result>=2);
    x = x + 1;
    return x;
}
