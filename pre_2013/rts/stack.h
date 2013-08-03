
#include "prim.h"


unit *stack;

#define PUSH(entry) *stack++ = entry
#define POP *--stack

