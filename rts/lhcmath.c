#include "master.h"

void show_mp(char *str, mp_int *mp) {
    char buf[1000];
    printf("%s: ", str);
    mp_toradix(mp, buf, 10);
    printf("%s\n", buf);
}

mp_int *lhc_mp_from_int(sunit i) {
    mp_int *mp;
    mp_int *lo_mp;
    mp_int *hi_mp,*hi_shifted_mp;
    sunit out;
    sunit lo, hi;
    hi = labs(i) >> 32;
    lo = labs(i);

    lo_mp = (mp_int*) GC_MALLOC(sizeof(mp_int));
    mp_init_set_int(lo_mp,lo);

    hi_mp = (mp_int*) GC_MALLOC(sizeof(mp_int));
    mp_init_set_int(hi_mp,hi);

    hi_shifted_mp = (mp_int*) GC_MALLOC(sizeof(mp_int));
    mp_init(hi_shifted_mp);

    mp_mul_2d(hi_mp, 32, hi_shifted_mp);

    mp = (mp_int*) GC_MALLOC(sizeof(mp_int));
    mp_init(mp);
    mp_add(lo_mp,hi_shifted_mp,mp);

    return mp;
}

sunit lhc_mp_get_int(mp_int *mp) {
    int sign;
    sunit lo;
    sunit hi;
    mp_int *tmp_mp = GC_MALLOC(sizeof(mp_int));
    mp_int *unused = GC_MALLOC(sizeof(mp_int));

    sign = SIGN(mp);
    lo = mp_get_int(mp);

    mp_init(tmp_mp);
    mp_init(unused);

    mp_div_2d(mp, 32, tmp_mp, unused);
    hi = mp_get_int(tmp_mp);
    return (lo + (hi<<32)) * (sign == 0 ? 1 : -1);
}

unit* lhc_mp_to_double(mp_int *mp, int e) {
    double val = (double) lhc_mp_get_int(mp);
    return doubleToWord(ldexp(val, e));
}

mp_int *lhc_mp_mul(mp_int *a, mp_int *b) {
    mp_int *mp;
    mp = (mp_int*) GC_MALLOC(sizeof(mp_int));
    mp_init(mp);
    mp_mul(a,b,mp);
    return mp;
}

mp_int *lhc_mp_add(mp_int *a, mp_int *b) {
    mp_int *mp;
    mp = (mp_int*) GC_MALLOC(sizeof(mp_int));
    mp_init(mp);
    mp_add(a,b,mp);
    return mp;
}

mp_int *lhc_mp_sub(mp_int *a, mp_int *b) {
    mp_int *mp;
    mp = (mp_int*) GC_MALLOC(sizeof(mp_int));
    mp_init(mp);
    mp_sub(a,b,mp);
    return mp;
}

mp_int *lhc_mp_gcd(mp_int *a, mp_int *b) {
    mp_int *mp;
    mp = (mp_int*) GC_MALLOC(sizeof(mp_int));
    mp_init(mp);
    mp_gcd(a,b,mp);
    return mp;
}

mp_int *lhc_mp_quot(mp_int *a, mp_int *b) {
    mp_int *mp;
    mp_int rem;
    mp = (mp_int*) GC_MALLOC(sizeof(mp_int));
    mp_init(mp);
    mp_init(&rem);
    mp_div(a,b,mp,&rem);
    return mp;
}

mp_int *lhc_mp_rem(mp_int *a, mp_int *b) {
    mp_int *mod;
    mod = (mp_int*) GC_MALLOC(sizeof(mp_int));
    mp_init(mod);
    mp_mod(a,b,mod);
    return mod;
}

mp_int *lhc_mp_abs(mp_int *a) {
    mp_int *mp;
    mp = (mp_int*) GC_MALLOC(sizeof(mp_int));
    mp_init(mp);
    mp_abs(a,mp);
    return mp;
}

mp_int *lhc_mp_negate(mp_int *a) {
    mp_int *mp;
    mp = (mp_int*) GC_MALLOC(sizeof(mp_int));
    mp_init(mp);
    mp_neg(a,mp);
    return mp;
}

sunit lhc_mp_cmp(mp_int *a, mp_int *b) {
    return mp_cmp(a,b);
}
