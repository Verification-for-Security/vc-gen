function driver(newCount, oldCount) {
    assume(newCount != oldCount);
    var lock = 0;
    while (newCount != oldCount) {
        lock = 1;
        oldCount = newCount;
        if (newCount >= 1){
            lock     = 0;
            newCount = newCount - 1;
        }
    };
    assert(lock != 0);
}
