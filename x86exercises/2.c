long equal_asm(long a, long b);
long equal(long a, long b) { return a == b; }

int main() {
    __builtin_printf("equal(1,2)     = %ld\n", equal(1, 2));
    __builtin_printf("equal_asm(1,2) = %ld\n", equal_asm(1, 2));
    __builtin_printf("equal(3,3)     = %ld\n", equal(3, 3));
    __builtin_printf("equal_asm(3,3) = %ld\n", equal_asm(3, 3));
}
