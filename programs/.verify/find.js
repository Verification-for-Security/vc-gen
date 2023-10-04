function find(a, key, len) {
    assume(0<=len);
    var index = 0;
    var res = 0;
    while (index < len && res==0) {
        if (a[index] == key){res = 1;} else {index = index + 1;}
    }
    assert(res == 0 ||  a[index]==key);
    assert(res == 1 || index == len);
    assert(res == 1 || forall(k, k<0 || k>=len || a[k] != key));
}