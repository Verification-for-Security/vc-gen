function find(arr_a, key, len) {
    // assume the input array is sorted;
    assume(forall(j, forall(k, j<0 || j>=k || k>=len || arr_a[j]<= arr_a[k])));
    assume(len >=0);
    var low = 0;
    var high = len;
    var pos = -1;
    while (low < high && pos < 0) {
        var mid = (low + high) / 2;
        if (arr_a[mid] < key) {low = mid + 1;} 
        else {
            if (key < arr_a[mid]) {high = mid;} else {pos = mid;}
        }
    }
    assert(pos < 0 ||  arr_[pos]==key);
    assert(pos >= 0 || low==high);
    assert(pos >= 0 || forall(k, k<0 || k>=len || arr_[k] != key));
}
