#ifndef __OBJECTS_H__
#define __OBJECTS_H__

#include "common.h"

// move to common.h?
typedef struct {
  uint8_t prims;
  uint8_t ptrs;
} ObjectInfo;

enum Tag
  { Unit,  Leaf,  Branch, Zero,  Succ,  IntBranch, TAG_MAX};
static const ObjectInfo InfoTable[] =
  { {0,0}, {1,0}, {0,2},  {0,0}, {0,1}, {1,2} };

typedef struct {
  word n;
  hp left;
  hp right;
} MkIntBranch;

typedef struct {
} MkUnit;

typedef struct {
  word n;
} MkLeaf;

typedef struct {
  hp left;
  hp right;
} MkBranch;

typedef struct {
} MkZero;

typedef struct {
  hp next;
} MkSucc;

typedef union {
  // MkIndirection ind;
  MkUnit unit;
  MkLeaf leaf;
  MkBranch branch;
  MkZero zero;
  MkSucc succ;
  MkIntBranch intbranch;
} Object;

#endif
