void print(long a, long b) { __builtin_printf("(%ld,%ld)\n", a, b); }

void swap_args(long a, long b) { print(b, a); }
void swap_args_asm(long a, long b);

int main() {
    __builtin_printf("swap_args     (1,2): ");
    swap_args(1, 2);
    __builtin_printf("swap_args_asm (1,2): ");
    swap_args_asm(1, 2);
}
