function locks(x) {
    var lock = 0;
    if (x >= 1){ 
        lock = 1;
    };
    
    if (x >= 1){ 
        lock = 0; 
    };
    assert(lock==0);
}
