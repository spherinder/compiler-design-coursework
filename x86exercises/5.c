long swap_asm(long *a, long *b);
void swap(long *a, long *b) {
    int c = *a;
    *a = *b;
    *b = c;
}

int main() {
    long a = 1;
    long b = 2;
    __builtin_printf("a = %ld, b = %ld", a, b);
    swap(&a, &b);
    __builtin_printf(", swap(&a, &b), ");
    __builtin_printf("    a = %ld, b = %ld\n", a, b);
    a = 1;
    b = 2;
    __builtin_printf("a = %ld, b = %ld", a, b);
    swap_asm(&a, &b);
    __builtin_printf(", swap_asm(&a, &b), ");
    __builtin_printf("a = %ld, b = %ld\n", a, b);
}
