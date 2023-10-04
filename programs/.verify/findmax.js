function find(a, key, len) {
    assume(forall(k, a[k]>=0));
    var index = 0;
    var max = -1;
    while (index < len) {
        if (a[index] > max){max = a[index];}
        index = index + 1;
    }
    assert(forall(k, k<0 || k>=len || max >= a[k]));
}