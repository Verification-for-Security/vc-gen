function f(x) {
    requires(x>=0);
    ensures($result>=2);
    x = x + 2;
    return x;
}

 function c(x){
    requires(x>=0);
    ensures($result>=2);
    y = f(x);
    y = y-1;
    return y;
}
