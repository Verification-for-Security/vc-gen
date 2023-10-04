// // This test case checks wether you correctly substitute the return value for a fresh variable.
// // You should not be able to verify this.
function f(x) {
    ensures($result<0);
    return -1;
}

function g(x) {
    ensures($result>0);
    return 1;
}

 function main(x){
    x = f(x);
    y = g(x);
    // this should always fail;
    assert(1<0);
}
