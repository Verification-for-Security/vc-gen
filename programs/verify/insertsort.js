function sort(a, len) {
    // This function implements insertion sort. You can find a description of
    // the algorithm here:
    // https://www.cs.cornell.edu/courses/cs2112/2020fa/lectures/lecture.html?id=sorting.
    // The site also contains a textual description of the invariant which may
    // be useful. Remember: this site only sketches the invariant informally. To
    // convince the verifier, you need to make it formal.

    assume(len>=1);

    var index=1;
    while (index<len){
        var key = a[index];
        var j = index;
        while(j>0 && a[j-1] > key){
            a[j] = a[j-1];
            j = j-1;
        }
        a[j]=key;
        index = index + 1;
    }
    // prove the array is sorted.
    assert(forall(l, forall(k, l<0 || l>=k || k>=len || a[l]<= a[k])));
}