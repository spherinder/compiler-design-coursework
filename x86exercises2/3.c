long sum_last_args(long arg1, long arg2, long arg3, long arg4, long arg5, long arg6, long arg7, long arg8){
    return arg6+arg7+arg8;
}
long sum_last_args_asm(long arg1, long arg2, long arg3, long arg4, long arg5, long arg6, long arg7, long arg8);

int main() {
    __builtin_printf("sum_last_arg(1,2,3,4,5,6,7,8):     %ld\n", sum_last_args(1,2,3,4,5,6,7,8));
    __builtin_printf("sum_last_arg_asm(1,2,3,4,5,6,7,8): %ld\n", sum_last_args_asm(1,2,3,4,5,6,7,8));
}
