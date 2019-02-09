#ifndef __UTILS_H__
#define __UTILS_H__

#include "header.h"
#include "objects.h"

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


#endif
