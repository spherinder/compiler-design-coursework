long sign_asm(long a);
long sign(long a) {
    if (a > 0)
        return 1;
    if (a < 0)
        return -1;
    return 0;
}

int main() {
    __builtin_printf("sign(3)       = %ld\n", sign(3));
    __builtin_printf("sign_asm(3)   = %ld\n", sign_asm(3));
    __builtin_printf("sign(0)       = %ld\n", sign(0));
    __builtin_printf("sign_asm(0)   = %ld\n", sign_asm(0));
    __builtin_printf("sign(-10)     = %ld\n", sign(-10));
    __builtin_printf("sign_asm(-10) = %ld\n", sign_asm(-10));
}
