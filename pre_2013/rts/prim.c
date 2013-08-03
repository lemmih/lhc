#include "master.h"

extern int global_argc;
extern char** global_argv;
extern int showGCInfo;

void init_args(int argc, char **argv) {
  int i,n;
  global_argv = (char **) malloc (argc * sizeof(char*));
  showGCInfo = 0;
  for(i=0,n=0;i<argc;i++) {
    if(strcmp(argv[i], "--show-gc-stats")==0) {
      showGCInfo = 1;
    } else {
      global_argv[n++] = argv[i];
    }
  }
  global_argc = n;
}

void getProgArgv(int *argc, char ***argv) {
  *argc = global_argc;
  *argv = global_argv;
}

double wordToDouble(unit *x) {
  DoubleOrUnit u;
  u.w = (u64) x;
  return u.d;
}

float wordToFloat(unit *x) {
  FloatOrUnit u;
  u.w = (u32) x;
  return u.d;
}

unit *doubleToWord(double x) {
  DoubleOrUnit u;
  u.d = x;
  return (unit*) u.w;
}

unit *floatToWord(float x) {
  FloatOrUnit u;
  u.d = x;
  return (unit*) u.w;
}

int __hscore_get_errno(void) {
    return errno;
}
ssize_t __hscore_PrelHandle_write(int fd, void *ptr, int offset, size_t count) {
    return write(fd, ptr + offset, count);
}
void *__hscore_memcpy_dst_off(void *dest, int offset, void *src, size_t n) {
    return memcpy(dest+offset, src, n);
}

#define INT_FLAG(hs_name,flag) \
  int hs_name(void) { \
     return flag;\
  }

INT_FLAG(__hscore_o_rdonly, O_RDONLY)
INT_FLAG(__hscore_o_wronly, O_WRONLY)
INT_FLAG(__hscore_o_rdwr, O_RDWR)
INT_FLAG(__hscore_o_append, O_APPEND)
INT_FLAG(__hscore_o_creat, O_CREAT)
INT_FLAG(__hscore_o_excl, O_EXCL)
INT_FLAG(__hscore_o_trunc, O_TRUNC)

INT_FLAG(__hscore_o_noctty, O_NOCTTY)


unit *rts_newArray(unit *ptr, unit value, unit size) {
    unit i;
    for(i = 0; i < size; i++) ptr[i] = value;
    return ptr;
}


// Stolen from the GHC RTS.
#define MY_DMINEXP  ((DBL_MIN_EXP) - (DBL_MANT_DIG) - 1)
/* DMINEXP is defined in values.h on Linux (for example) */
#define DHIGHBIT 0x00100000
#define DMSBIT   0x80000000

#define MY_FMINEXP  ((FLT_MIN_EXP) - (FLT_MANT_DIG) - 1)
#define FHIGHBIT 0x00800000
#define FMSBIT   0x80000000



void __decodeDouble_2Int (s32 *man_sign, u32 *man_high, u32 *man_low, s32 *exp, double dbl)
{
    /* Do some bit fiddling on IEEE */
    u32 low, high;
    s32 sign, iexp;
    union { double d; u32 i[2]; } u;


    u.d = dbl;    /* grab chunks of the double */
    low = u.i[0];
    high = u.i[1];

    if (low == 0 && (high & ~DMSBIT) == 0) {
        *man_low = 0;
        *man_high = 0;
        *exp = 0L;
    } else {
        iexp = ((high >> 20) & 0x7ff) + MY_DMINEXP;
        sign = high;

        high &= DHIGHBIT-1;
        if (iexp != MY_DMINEXP)/* don't add hidden bit to denorms */
            high |= DHIGHBIT;
        else {
            iexp++;
            /* A denorm, normalize the mantissa */
            while (! (high & DHIGHBIT)) {
                high <<= 1;
                if (low & DMSBIT)
                    high++;
                low <<= 1;
                iexp--;
            }
        }
        *exp = (s32) iexp;
        *man_low = low;
        *man_high = high;
        *man_sign = (sign < 0) ? -1 : 1;
    }
}

/* Convenient union types for checking the layout of IEEE 754 types -
   based on defs in GNU libc <ieee754.h>
*/

void __decodeFloat_Int (s32 *man, s32 *exp, double flt)
{
    ___decodeFloat_Int(man,exp,(float)flt);
}
void ___decodeFloat_Int (s32 *man, s32 *exp, float flt)
{
    /* Do some bit fiddling on IEEE */
    s32 high, sign;
    union { float f; s32 i; } u;


    u.f = flt;    /* grab the float */
    high = u.i;

    if ((high & ~FMSBIT) == 0) {
        *man = 0;
        *exp = 0;
    } else {
        *exp = ((high >> 23) & 0xff) + MY_FMINEXP;
        sign = high;

        high &= FHIGHBIT-1;
        if (*exp != MY_FMINEXP)/* don't add hidden bit to denorms */
            high |= FHIGHBIT;
        else {
            (*exp)++;
            /* A denorm, normalize the mantissa */
            while (! (high & FHIGHBIT)) {
                high <<= 1;
                (*exp)--;
            }
        }
        *man = high;
        if (sign < 0)
            *man = - *man;
    }
}

unit *__int2Double(unit *i)
{
    double f = (double) ((sunit)i);
    return doubleToWord(f);
}

unit *__int2Float(unit *i)
{
    float f = (float) ((sunit)i);
    return floatToWord(f);
}



void panic(char *str) {
  fputs(str,stderr);
  exit(1);
}

unit *newMutVar(unit *val) {
    unit *var;
    var = alloc(sizeof(unit*));
    var[0] = val;
    return var;
}
unit *readMutVar(unit *var) {
    return var[0];
}
void updateMutVar(unit *var, unit *val) {
    var[0] = val;
}

