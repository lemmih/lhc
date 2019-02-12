#ifndef __UTILS_H__
#define __UTILS_H__

#include "header.h"
#include "objects.h"
#include <sys/mman.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

void touchSpace(hp ptr, word size);

static Header readHeader(hp ptr) {
  return *(Header*)ptr;
}
static Object* readObject(hp ptr) {
  return (Object*)(ptr+1);
}

static void writeIndirection(hp from, hp to) {
  Header ind;
  ind.raw = 0;
  ind.forwardPtr = to;
  ind.data.isForwardPtr = 1;
  *from = ind.raw;
}

static void followIndirection(hp* objAddr) {
  hp obj = *objAddr;
  Header header = readHeader(obj);
  while( header.data.isForwardPtr ) {
    obj = (hp) ((word)header.forwardPtr & (~1));
    *objAddr = obj;
    header = readHeader(obj);
  }
}

static void* mmap_aligned(size_t alignment, size_t length, int prot, int flags) {
  word off;
  void *p = mmap(NULL, length+alignment, prot, flags, -1, 0);
  assert(p != (void*)-1);
  off = (word)p%alignment;
  if(off) {
    munmap(p, alignment-off);
    p += alignment - off;
  }
  return p;
}


#endif
