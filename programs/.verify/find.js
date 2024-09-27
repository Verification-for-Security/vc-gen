function find(arr_a, key, len) {
    assume(0<=len);
    var index = 0;
    var res = 0;
    while (index < len && res==0) {
        if (arr_a[index] == key){res = 1;} else {index = index + 1;}
    }
    assert(res == 0 ||  arr_a[index]==key);
    assert(res == 1 || index == len);
    assert(res == 1 || forall(k, k<0 || k>=len || arr_a[k] != key));
}
