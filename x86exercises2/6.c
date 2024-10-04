struct S1 {
    long a;
    long b;
};
struct S2 {
    long a;
    long b;
    long c;
};

struct S1 return_S1() {
    struct S1 s = {1, 2};
    return s;
}
struct S2 return_S2() {
    struct S2 s = {3, 4, 5};
    return s;
}
struct S1 return_S1_asm();
struct S2 return_S2_asm();

// long return_S2_c_via_call_asm(){
//    return return_S2().c;
// }
//
long return_S2_c_via_call_asm();

int main() {
    struct S1 s1 = return_S1();
    struct S1 s1_asm = return_S1_asm();
    __builtin_printf("return_S1:     %ld %ld\n", s1.a, s1.b);
    __builtin_printf("return_S1_asm: %ld %ld\n", s1_asm.a, s1_asm.b);

    struct S2 s2 = return_S2();
    struct S2 s2_asm = return_S2_asm();
    __builtin_printf("return_S2:     %ld %ld %ld\n", s2.a, s2.b, s2.c);
    __builtin_printf("return_S2_asm: %ld %ld %ld\n", s2_asm.a, s2_asm.b,
                     s2_asm.c);

    __builtin_printf("return_S2_c_via_call_asm: %ld\n",
                     return_S2_c_via_call_asm());
}
