struct S1{
    long a;
    long b;
};
struct S2{
    long a;
    long b;
    long c;
};

long sum_all_members_S1(struct S1 s) {
    return s.a + s.b;
}

long sum_all_members_S2(struct S2 s) {
    return s.a + s.b + s.c;
}

long sum_all_members_S1_asm(struct S1 s);
long sum_all_members_S2_asm(struct S2 s);


int main() {
    struct S1 s1 = {1, 2};
    struct S2 s2 = {3, 4, 5};
    __builtin_printf("sum_all_members_S1:     %ld\n", sum_all_members_S1(s1));
    __builtin_printf("sum_all_members_S1_asm: %ld\n", sum_all_members_S1_asm(s1));
    __builtin_printf("sum_all_members_S2:     %ld\n", sum_all_members_S2(s2));
    __builtin_printf("sum_all_members_S2_asm: %ld\n", sum_all_members_S2_asm(s2));
}
