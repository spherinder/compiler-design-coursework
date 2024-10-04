long add_asm(long a, long b);
long add(long a, long b) { return a + b; }

int main() {
    __builtin_printf("add(1,2)      = %ld\n", add(1, 2));
    __builtin_printf("add_asm(1,2)  = %ld\n", add_asm(1, 2));
    __builtin_printf("add(-2,3)     = %ld\n", add(-2, 3));
    __builtin_printf("add_asm(-2,3) = %ld\n", add_asm(-2, 3));
}
