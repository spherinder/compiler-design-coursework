long sum_abs(long *arr, long n){
    long sum = 0l;
    for (long i = 0l; i < n; ++i) {
        if (arr[i] >= 0l)
            sum += arr[i];
        else 
            sum -= arr[i];
    }
    return sum;
}


long func(long *arr, long n);


int main(int argc, char *argv[]){
    long arr[5] = {1l, -2l, 3l, -4l, 5l};
    __builtin_printf("%ld\n", func(arr, 5l));
    return 0;
}
