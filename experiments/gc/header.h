
#ifndef __HEADER_H__
#define __HEADER_H__

#include "objects.h"
#include <stdbool.h>
#include <assert.h>

typedef union {
  hp forwardPtr;
  struct {
    bool isForwardPtr:1;
    unsigned int gen:2;
    bool grey:1;
    unsigned int black:1;
    unsigned int tag:15;
    bool mutable:1;
    unsigned int marked_ptrs:5;
    unsigned int unused:6;
    // not available on 32bit systems:
    unsigned int prims:5;
    unsigned int ptrs:5;
  } data;
  word raw;
} Header;

inline static uint8_t getObjectPrims(Header h) {
  assert(h.data.prims == InfoTable[h.data.tag].prims);
  return h.data.prims;
  // return InfoTable[h.data.tag].prims;
}
inline static uint8_t getObjectPtrs(Header h) {
  assert(h.data.ptrs == InfoTable[h.data.tag].ptrs);
  return h.data.ptrs;
  // return InfoTable[h.data.tag].ptrs;
}

#endif
