function max(x, y) { 
    var r = 0;
    if (x >= y+1) {
        r = x;
    } else {
        r = y;
    }
    assert(r >= x);
    assert(r >= y);
    assert(r >= y+1);
}
