#include <stdlib.h>
void reverse_array2(long *a, long n);

int main() {
    long a[10] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    reverse_array2(a, 10);
    __builtin_printf("%ld, %ld, %ld, %ld, %ld, %ld, %ld, %ld, %ld, %ld\n", a[0],
                     a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9]);

    long n = 10000000;
    long *b = malloc(n * sizeof(long));
    for (long i = 0; i < n; i++) {
        b[i] = i;
    }
    reverse_array2(b, n);
    __builtin_printf("%ld, %ld, %ld, %ld, %ld, %ld, %ld, %ld, %ld, %ld\n", b[0],
                     b[1], b[2], b[3], b[4], b[5], b[6], b[7], b[8], b[9]);
}
