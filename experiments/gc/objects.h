#ifndef __OBJECTS_H__
#define __OBJECTS_H__

#include "common.h"

// move to common.h?

enum Tag
  { Unit,  Leaf,  Branch, Zero,  Succ,  IntBranch, IORef, TAG_MAX};

typedef struct {
  enum Tag tag;
  uint8_t prims;
  uint8_t ptrs;
} ObjectInfo;

static const ObjectInfo InfoTable[] =
  { {Unit,0,0}, {Leaf,1,0}, {Branch,0,2}, {Zero,0,0}, {Succ,0,1}
  , {IntBranch,1,2}
  , {.tag=IORef,.prims=0,.ptrs=1} };

typedef struct {
  hp ptr;
} MkIORef;

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
  MkIORef ioref;
} Object;

#endif
