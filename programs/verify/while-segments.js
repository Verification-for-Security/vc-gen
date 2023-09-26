function foo() {
    var x = 0;
    var y = 50;
    while (x <= 99) {
        x = x + 1;

        if (x >= 51) {
            y = y + 1;
        }
    }
    assert (y == 100);
}

// I ^ b s I
// {(x <= 99) ^ (y >= x and y < 100)} x = x + 1 ; if... ; y = y + 1 {(y >= x and y < 100)}
// why this example pass?
// Non-exhaustive patterns in function toNanoBexp ？？？