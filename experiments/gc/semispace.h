#ifndef __SEMISPACE_H__
#define __SEMISPACE_H__

#include "common.h"
#include "stats.h"
#include "utils.h"
#include <stdbool.h>
#include <assert.h>

#define MIN_AREA (1024*1024/sizeof(word))

typedef struct {
  unsigned int black_bit:1;
  double factor;
  bool has_roots;

  struct {
    hp ptr1;
    hp ptr2;
    int size;
  } white_space;

  struct {
    hp ptr;
    hp free;
    hp scavenged;
    int size;
  } grey_space;

  struct {
    hp ptr;
    hp free;
    int size;
  } black_space;
} SemiSpace;

#define area_limit(area) (area.ptr+area.size)
#define area_used(area) (area.free-area.ptr)
void semi_init(SemiSpace *semi);
void semi_close(SemiSpace *semi, Stats*);

bool semi_check(SemiSpace *semi, word size);

void semi_scavenge(SemiSpace *semi, Stats*);
void semi_scavenge_concurrent(SemiSpace *semi, Stats *s);
word semi_size(SemiSpace *semi);

#define IS_WHITE(semi, header) (!(IS_BLACK(semi,header) || IS_GREY(header)))
#define IS_BLACK(semi, header) ((header).data.black == (semi)->black_bit)
#define IS_GREY(header) ((header).data.grey)

#define IS_IN_AREA(area, o) (o >= area.ptr && o <= area.free)

static hp semi_bump_black(SemiSpace *semi, const word size) {
  hp ret;
  assert(semi->black_space.free+size <= area_limit(semi->black_space));
  ret = semi->black_space.free;
  semi->black_space.free += size;
  return ret;
}

static hp semi_bump_grey(SemiSpace *semi, const word size) {
  hp ret;
  assert(semi->grey_space.free+size <= area_limit(semi->grey_space));
  // __builtin_assume(semi->grey_space.free != NULL);
  ret = semi->grey_space.free;
  // if(!ret) abort(); // clang spills something if this is removed. :-/
  // __builtin_assume(ret != NULL);
  semi->grey_space.free += size;
  return ret;
}


inline static void semi_evacuate(SemiSpace *semi, hp* objAddr) {
  hp obj = *objAddr;
  Header header;

  header = readHeader(obj);
  while( header.data.isForwardPtr ) {
    obj = (hp) ((word)header.forwardPtr & (~1));
    *objAddr = obj;
    header = readHeader(obj);
  }
  assert( header.data.isForwardPtr == 0 );

  const uint8_t prims = header.data.prims;
  const uint8_t ptrs = header.data.ptrs;
  const word obj_size = 1+prims+ptrs;

  assert( header.data.gen == 1 );
  if( !IS_WHITE(semi, header) ) return;

  header.data.grey = true;
  header.data.black = !semi->black_bit;

  const hp dst = semi_bump_grey(semi, obj_size);
  #ifdef CLANG
  __builtin_assume(dst != NULL);
  #endif

  *objAddr = dst;
  writeIndirection(obj, dst);

  *dst = header.raw;
  #pragma clang loop unroll_count(1)
  for(int i=1;i<obj_size;i++) {
    dst[i] = obj[i];
  }
}


inline static hp semi_evacuate_ret(hp* objAddr, unsigned int black_bit, bool temporal, hp grey) {
  hp obj = *objAddr;
  Header header;
  word old_header;

retry:
  if(temporal)
    header = readHeader(obj);
  else
    header.raw = __builtin_nontemporal_load(obj);
  while( header.data.isForwardPtr ) {
    obj = (hp) ((word)header.forwardPtr & (~1));
    *objAddr = obj;
    if(temporal)
      header = readHeader(obj);
    else
      header.raw = __builtin_nontemporal_load(obj);
  }
  old_header = header.raw;
  assert( header.data.isForwardPtr == 0 );

  const uint8_t prims = header.data.prims;
  const uint8_t ptrs = header.data.ptrs;
  const word obj_size = 1+prims+ptrs;

  assert( header.data.gen == 1 );
  if( IS_GREY(header) || header.data.black == black_bit ) return grey;
  // if( !IS_WHITE(semi, header) ) return grey;

  header.data.grey = true;
  header.data.black = !black_bit;

  const hp dst = grey;
  grey += obj_size;


  if(temporal) {
    if(InfoTable[header.data.tag].mutable) {
      Header whitehole;
      whitehole.raw = 0;
      whitehole.data.tag = Whitehole;
      if(!__sync_bool_compare_and_swap (obj, old_header, whitehole.raw)) {
        printf("CAS failed.\n");
        goto retry;
      }
    }
    *dst = header.raw;
    #pragma clang loop unroll_count(1)
    for(int i=1;i<obj_size;i++) {
      dst[i] = obj[i];
    }
    writeIndirection(obj, dst);
  } else {
    // XXX: This code doesn't support mutable objects.
    __builtin_nontemporal_store(header.raw,dst);
    // #pragma clang loop unroll_count(1)
    for(int i=1;i<obj_size;i++) {
      word val = __builtin_nontemporal_load(obj+i);
      __builtin_nontemporal_store(val,dst+i);
    }
    {
      Header ind;
      ind.raw = 0;
      ind.forwardPtr = dst;
      ind.data.isForwardPtr = 1;
      // __builtin_nontemporal_store(ind.raw,obj);
    }
  }

  *objAddr = dst;
  // writeIndirection(obj, dst);

  return grey;
}

#endif
