function foo() {
    var x = 0;
    var y = 50;
    while (x <= 99) {
        x = x + 1;

        if (x >= 51) {
            y = y + 1;
        }
    }
    assert(y == 100);
}
