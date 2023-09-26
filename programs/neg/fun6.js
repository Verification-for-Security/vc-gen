// This test case checks wether you correctly substitute the return value for a fresh variable.
// You should not be able to verify this.
function f(x,y) {
    ensures($result<0);
    if (y>0){return 1;}
    return -1;
}
