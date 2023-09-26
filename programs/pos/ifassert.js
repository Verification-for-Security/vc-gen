function ifassert(x,y) {
    var lock = 0;
    if (x >= 1){ 
        lock = 1;
        assert(lock >=1 && x>=1);
    };
    
    if (y >= 1){ 
        lock = 0; 
        assert(lock <=0 && y>=1);
    };
}
