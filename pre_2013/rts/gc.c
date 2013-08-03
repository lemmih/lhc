#include "master.h"

#ifndef LHC_GC_BLOCK_SIZE
#define LHC_GC_BLOCK_SIZE 128
#endif

#ifndef LHC_GC_GRACE_BUFFER
#error GRACE BUFFER size not defined.
#endif

#define LHC_GC_BLOCK_SIZE_BYTES (LHC_GC_BLOCK_SIZE * sizeof(unit))
#define LHC_GC_GRACE_BUFFER_BYTES (LHC_GC_GRACE_BUFFER * sizeof(unit))

/*
void* alloc(int size) {
  static void *block=NULL;
  static int allocated = 0;
  void *ret;
  if(block==NULL) {
     block = GC_MALLOC(LHC_GC_BLOCK_SIZE_BYTES);
  }
  if(allocated + size + LHC_GC_GRACE_BUFFER_BYTES > LHC_GC_BLOCK_SIZE_BYTES) {
    block = GC_MALLOC(LHC_GC_BLOCK_SIZE_BYTES);
    allocated = 0;
  }
  ret = block+allocated;
  allocated += size;
  return ret;
}
*/
/*
void *alloc(int size) {
  static void *block=NULL;
  static int allocated = 0;
  void *ret;
  if(block==NULL) {
    block = malloc(LHC_GC_BLOCK_SIZE_BYTES);
  }
  if(allocated + size + LHC_GC_GRACE_BUFFER_BYTES > LHC_GC_BLOCK_SIZE_BYTES) {
    block = malloc(LHC_GC_BLOCK_SIZE_BYTES);
    if(block == NULL) panic("OOM");
    allocated = 0;
  }
  ret = block+allocated;
  allocated += size;
  return ret;
}
*/
void *alloc(int size) {
  return GC_MALLOC(size < LHC_GC_GRACE_BUFFER_BYTES ? LHC_GC_GRACE_BUFFER_BYTES : size);
}
