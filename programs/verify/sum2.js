function sum(n) {
    var i = 1; 
    var sum = 0;  
    while (n >= i) {
        var j = 1;
        while (i >= j) {
            sum = sum + j;
            j = j + 1;
            i = i + 1;
        }
        assert (sum >= 0);
    }
}


