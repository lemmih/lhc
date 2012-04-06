#ifndef __LHC_PRIM__
#define __LHC_PRIM__

typedef unsigned long u64;
typedef unsigned int u32;
typedef unsigned short u16;
typedef unsigned char u8;
typedef signed long s64;
typedef signed int s32;
typedef signed short s16;
typedef signed char s8;

typedef u64 unit;
typedef s64 sunit;

typedef union { float d; u32 w; } FloatOrUnit;
typedef union { double d; u64 w; } DoubleOrUnit;

int global_argc;
char** global_argv;

int showGCInfo;

void init_args(int argc, char **argv);

double wordToDouble(unit *x);
unit *doubleToWord(double x);

float wordToFloat(unit *x);
unit *floatToWord(float x);

unit *__int2Double(unit *i);
unit *__int2Float(unit *i);

void __decodeFloat_Int (s32 *man, s32 *exp, double flt);
void __decodeDouble_2Int (s32 *man_sign, u32 *man_high, u32 *man_low, s32 *exp, double dbl);
void ___decodeFloat_Int (s32 *man, s32 *exp, float flt);

void getProgArgv(int *argc, char ***argv);
int __hscore_get_errno(void);
ssize_t __hscore_PrelHandle_write(int fd, void *ptr, int offset, size_t count);
void *__hscore_memcpy_dst_off(void *dest, int offset, void *src, size_t n);
unit *rts_newArray(unit *ptr, unit value, unit size);

void panic(char *str);

#endif
