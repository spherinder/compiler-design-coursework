long ackermann(long m, long n) {
    if (m == 0) {
        return n + 1;
    } else if (n == 0) {
        return ackermann(m - 1, 1);
    } else {
        long x = ackermann(m, n - 1);
        return ackermann(m - 1, x);
    }
}

long ackermann_asm(long m, long n);

int main() {
    __builtin_printf("ackermann(1,3):     %ld\n", ackermann(1, 3));
    __builtin_printf("ackermann_asm(1,3): %ld\n", ackermann_asm(1, 3));
    __builtin_printf("ackermann(3,2):     %ld\n", ackermann(3, 2));
    __builtin_printf("ackermann_asm(3,2): %ld\n", ackermann_asm(3, 2));
}
