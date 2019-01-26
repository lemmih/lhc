#ifndef __COMMON_H__
#define __COMMON_H__

#include <stdint.h>

typedef uint64_t word;
typedef word* hp;

#define MAX(a,b) \
  ({ __typeof__ (a) _a = (a); \
     __typeof__ (b) _b = (b); \
     _a > _b ? _a : _b; })

#endif
