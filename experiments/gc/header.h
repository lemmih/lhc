
#ifndef __HEADER_H__
#define __HEADER_H__

#include <stdbool.h>

typedef union {
  hp forwardPtr;
  struct {
    bool isForwardPtr:1;
    unsigned int gen:2;
    bool grey:1;
    unsigned int black:1;
    unsigned int tag:10;
    unsigned int prims:5;
    unsigned int ptrs:5;
    bool mutable:1;
    // 1+2+1+1+10+5+1 = 21 bits
  } data;
  word raw;
} Header;

#endif
