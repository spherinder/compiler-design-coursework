#include <stdlib.h>
void reverse_array(long *a, long n);

int main() {
    long a[5] = {1, 2, 3, 4, 5};
    reverse_array(a, 5);
    __builtin_printf("%ld %ld %ld %ld %ld\n", a[0], a[1], a[2], a[3], a[4]);

    //    long n = 100000000;
    //    long *b = malloc(n * sizeof(long));
    //    for (long i = 0; i < n; i++) {
    //        b[i] = i;
    //    }
    //    reverse_array(b, n);
}
