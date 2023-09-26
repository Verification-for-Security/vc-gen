function inc(x,j,i) {
    a[i] = x
    b[i] = a[i]+1;
    b[j] = 3; 
    assert (b[i]==a[i]+1);
}
