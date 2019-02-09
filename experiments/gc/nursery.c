#include "common.h"
#include "objects.h"
#include "nursery.h"
#include "header.h"
#include "semispace.h"
#include "utils.h"
#include "stats.h"
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdbool.h>

inline static void nursery_evacuate_one(Nursery *ns, SemiSpace *semi, hp* objAddr);
static hp nursery_evacuate_plain(Nursery *ns, SemiSpace *semi, hp* objAddr, hp);
static void nursery_evacuate_noroots(Nursery *ns, SemiSpace *semi, hp* objAddr);
static void nursery_evacuate_bypass(Nursery *ns, SemiSpace *semi, hp* objAddr);
static void nursery_evacuate_plain_stack(Nursery *ns, SemiSpace *semi, hp* stack[], int idx);
static void nursery_evacuate_stack(Nursery *ns, SemiSpace *semi, hp* objAddr);

void nursery_begin(Nursery *ns, SemiSpace *semi, Stats *s) {
  ns->black_start = semi->black_space.free;
  stats_timer_begin(s, Gen0Timer);
}

inline static void nursery_evacuate_one_copy(Nursery *ns, SemiSpace *semi, hp* objAddr) {
  hp obj = *objAddr;

  if(!nursery_member(ns, obj)) {
    if(IS_IN_AREA(semi->black_space, obj) || IS_IN_AREA(semi->grey_space, obj)) {
      return;
    }
  }

  Header header = readHeader(obj);
  while( header.data.isForwardPtr ) {
    obj = (hp) ((word)header.forwardPtr & (~1));
    *objAddr = obj;
    if(!nursery_member(ns, obj)) {
      if(IS_IN_AREA(semi->black_space, obj) || IS_IN_AREA(semi->grey_space, obj)) {
        return;
      }
    }
    header = readHeader(obj);
  }
  assert( header.data.isForwardPtr == 0);

  const uint8_t prims = header.data.prims;
  const uint8_t ptrs = header.data.ptrs;
  assert(prims == InfoTable[header.data.tag].prims);
  assert(ptrs == InfoTable[header.data.tag].ptrs);
  const word obj_size = 1+prims+ptrs;

  switch( header.data.gen ) {
    case 0: {
      const hp dst = semi_bump_black(semi, obj_size);
      header.data.black = semi->black_bit;
      header.data.gen = 1;

      *objAddr = dst;
      writeIndirection(obj, dst);

      *dst = header.raw;
      #pragma clang loop unroll_count(1)
      for(int i=1;i<obj_size;i++) {
        dst[i] = obj[i];
      }
      return;
    }
    case 1:
      assert( IS_WHITE(semi, header) );

      const hp dst = semi_bump_grey(semi, obj_size);
      header.data.grey = true;
      header.data.black = !semi->black_bit;

      *objAddr = dst;
      writeIndirection(obj, dst);

      *dst = header.raw;
      #pragma clang loop unroll_count(1)
      for(int i=1;i<obj_size;i++) {
        dst[i] = obj[i];
      }

      return;
    default:
      __builtin_unreachable();
      // abort();
  }
}

// Faster with breadth-first:
// 2c
//   2c has huge number of branches.
// Faster with depth-first:
// 1 2a 4

void nursery_scavenge(Nursery *ns, SemiSpace *semi) {
  Header header;
  hp start=ns->black_start, scavenged = ns->black_start;
  word evacuated=0;
  while(scavenged < semi->black_space.free) {
    header = readHeader(scavenged);

    const uint8_t prims = header.data.prims;
    const uint8_t ptrs = header.data.ptrs;
    scavenged += 1 + prims;
    for(int i=0;i<ptrs;i++) {
      nursery_evacuate_one_copy(ns, semi, (hp*) scavenged);
      scavenged++;
    }
  }
}

// static hp black_free;

// BFS:   37.956s
// Rec:   35.500s
// Stack: 33.591s

// Depth-first evacuation of objects from nursery to semi-space.
// Semi-space is garanteed to have enough space. No need to check.
// Objects from nursery are moved to black area and immediately
// scavenged. Reachable white objects from semi-space are evacuated
// to grey space and not scavenged.
void nursery_evacuate(Nursery *ns, SemiSpace *semi, hp* objAddr) {
  #ifdef BreadthFirstSearch
  return nursery_evacuate_one(ns, semi, objAddr);
  #else
  // semi->black_space.free = nursery_evacuate_plain(ns, semi, objAddr,semi->black_space.free);
  nursery_evacuate_stack(ns, semi, objAddr);
  #endif
  // if(ns->bypass) {
  //   return nursery_evacuate_bypass(ns, semi, objAddr);
  // } else {

  // }
}

inline static void nursery_evacuate_one(Nursery *ns, SemiSpace *semi, hp* objAddr) {
  hp obj = *objAddr;

  if(!nursery_member(ns, obj)) {
    if(IS_IN_AREA(semi->black_space, obj) || IS_IN_AREA(semi->grey_space, obj)) {
      return;
    }
  }

  Header header = readHeader(obj);
  while( header.data.isForwardPtr ) {
    obj = (hp) ((word)header.forwardPtr & (~1));
    *objAddr = obj;
    if(!nursery_member(ns, obj)) {
      if(IS_IN_AREA(semi->black_space, obj) || IS_IN_AREA(semi->grey_space, obj)) {
        return;
      }
    }
    header = readHeader(obj);
  }
  assert( header.data.isForwardPtr == 0);

  const uint8_t prims = header.data.prims;
  const uint8_t ptrs = header.data.ptrs;
  assert(prims == InfoTable[header.data.tag].prims);
  assert(ptrs == InfoTable[header.data.tag].ptrs);
  const word obj_size = 1+prims+ptrs;

  switch( header.data.gen ) {
    case 0: {
      const hp dst = semi_bump_black(semi, obj_size);
      header.data.black = semi->black_bit;
      header.data.gen = 1;

      *objAddr = dst;
      writeIndirection(obj, dst);

      *dst = header.raw;
      #pragma clang loop unroll_count(1)
      for(int i=1;i<obj_size;i++) {
        dst[i] = obj[i];
      }

      return;
    }
    case 1:
      assert( IS_WHITE(semi, header) );

      const hp dst = semi_bump_grey(semi, obj_size);
      header.data.grey = true;
      header.data.black = !semi->black_bit;

      *objAddr = dst;
      writeIndirection(obj, dst);

      *dst = header.raw;
      #pragma clang loop unroll_count(1)
      for(int i=1;i<obj_size;i++) {
        dst[i] = obj[i];
      }
      return;
    default:
      __builtin_unreachable();
      // abort();
  }
}

#define STACK_MAX (NURSERY_SIZE/2)
static void nursery_evacuate_stack(Nursery *ns, SemiSpace *semi, hp* objAddr) {
  hp* stack[STACK_MAX];
  int top = -1;
  const unsigned int black_bit = semi->black_bit;
  const hp black_ptr = semi->black_space.ptr;
  hp black_free = semi->black_space.free;
  const hp grey_ptr = semi->grey_space.ptr;
  hp grey_free = semi->grey_space.free;

  stack[++top] = objAddr;

repeat:
  while(top != -1) {
    objAddr = stack[top--];
    hp obj = *objAddr;
cycle:

    if(!nursery_member(ns, obj)) {
      if((obj >= black_ptr && obj <= black_free) || (obj >= grey_ptr && obj <= grey_free)) {
        goto repeat;
      }
    }

    Header header = readHeader(obj);
    while( header.data.isForwardPtr ) {
      obj = (hp) ((word)header.forwardPtr & (~1));
      *objAddr = obj;
      if(!nursery_member(ns, obj)) {
        if((obj >= black_ptr && obj <= black_free) || (obj >= grey_ptr && obj <= grey_free)) {
          goto repeat;
        }
      }
      header = readHeader(obj);
    }
    assert( header.data.isForwardPtr == 0);

    const uint8_t prims = header.data.prims;
    const uint8_t ptrs = header.data.ptrs;
    // const uint8_t prims = InfoTable[header.data.tag].prims;
    // const uint8_t ptrs = InfoTable[header.data.tag].ptrs;
    assert(prims == InfoTable[header.data.tag].prims);
    assert(ptrs == InfoTable[header.data.tag].ptrs);
    const word obj_size = 1+prims+ptrs;

    switch( header.data.gen ) {
      case 0: {
        const hp dst = black_free;
        black_free += obj_size;
        header.data.black = black_bit;
        header.data.gen = 1;

        *objAddr = dst;
        writeIndirection(obj, dst);

        *dst = header.raw;
        #pragma clang loop unroll_count(1)
        for(int i=1;i<obj_size;i++) {
          dst[i] = obj[i];
        }

        switch(ptrs) {
          case 0: break;
          case 1:
            objAddr = (hp*) dst+1+prims;
            obj = *objAddr;
            goto cycle;
          case 2:
            stack[++top] = ((hp*) dst+1+prims);
            objAddr = (hp*) dst+1+prims+1;
            obj = *objAddr;
            goto cycle;
          default:
            for(int i=0;i<ptrs-1;i++)
              stack[++top] = ((hp*) dst+1+prims+i);
            break;
        }
        break;
      }
      case 1:
        assert( IS_WHITE(semi, header) );

        const hp dst = grey_free;
        grey_free += obj_size;
        header.data.grey = true;
        header.data.black = !black_bit;

        *objAddr = dst;
        writeIndirection(obj, dst);

        *dst = header.raw;
        #pragma clang loop unroll_count(1)
        for(int i=1;i<obj_size;i++) {
          dst[i] = obj[i];
        }
        break;
      default:
        __builtin_unreachable();
        // abort();
    }
  }
  semi->black_space.free = black_free;
  semi->grey_space.free = grey_free;
}

static hp nursery_evacuate_plain(Nursery *ns, SemiSpace *semi, hp* objAddr, hp black_free) {
  hp obj = *objAddr;

  if(!nursery_member(ns, obj)) {
    if((obj >= semi->black_space.ptr && obj <= black_free) || IS_IN_AREA(semi->grey_space, obj)) {
    // if(IS_IN_AREA(semi->black_space, obj) || IS_IN_AREA(semi->grey_space, obj)) {
      return black_free;
    }
  }

  Header header = readHeader(obj);
  while( header.data.isForwardPtr ) {
    obj = (hp) ((word)header.forwardPtr & (~1));
    *objAddr = obj;
    if(!nursery_member(ns, obj)) {
      // if(IS_IN_AREA(semi->black_space, obj) || IS_IN_AREA(semi->grey_space, obj)) {
      if((obj >= semi->black_space.ptr && obj <= black_free) || IS_IN_AREA(semi->grey_space, obj)) {
        return black_free;
      }
    }
    header = readHeader(obj);
  }
  assert( header.data.isForwardPtr == 0);

  const uint8_t prims = header.data.prims;
  const uint8_t ptrs = header.data.ptrs;
  assert(prims == InfoTable[header.data.tag].prims);
  assert(ptrs == InfoTable[header.data.tag].ptrs);
  const word obj_size = 1+prims+ptrs;

  switch( header.data.gen ) {
    case 0: {
      // const hp dst = semi_bump_black(semi, obj_size);
      const hp dst = black_free;
      black_free += obj_size;
      header.data.black = semi->black_bit;
      header.data.gen = 1;

      *objAddr = dst;
      writeIndirection(obj, dst);

      *dst = header.raw;
      #pragma clang loop unroll_count(1)
      for(int i=1;i<obj_size;i++) {
        dst[i] = obj[i];
      }

      switch(ptrs) {
        case 0: return black_free;
        case 1: return nursery_evacuate_plain(ns, semi, (hp*) dst+1+prims, black_free);
        case 2:
          objAddr = (hp*) dst+1+prims+1;
          black_free = nursery_evacuate_plain(ns, semi, (hp*) dst+1+prims, black_free);
          return nursery_evacuate_plain(ns, semi, objAddr, black_free);
        default:
          for(int i=0;i<ptrs;i++)
            black_free = nursery_evacuate_plain(ns, semi, (hp*) dst+1+prims+i, black_free);
          return black_free;
      }}
    case 1:
      assert( IS_WHITE(semi, header) );

      const hp dst = semi_bump_grey(semi, obj_size);
      header.data.grey = true;
      header.data.black = !semi->black_bit;

      *objAddr = dst;
      writeIndirection(obj, dst);

      *dst = header.raw;
      #pragma clang loop unroll_count(1)
      for(int i=1;i<obj_size;i++) {
        dst[i] = obj[i];
      }
      return black_free;
    default:
      __builtin_unreachable();
      // abort();
  }
}

static void nursery_evacuate_bypass(Nursery *ns, SemiSpace *semi, hp* objAddr) {
  hp obj = *objAddr;
  Header header;

  header = readHeader(obj);
  while( header.data.isForwardPtr ) {
    obj = (hp) ((word)header.forwardPtr & (~1));
    *objAddr = obj;
    header = readHeader(obj);
  }
  assert( header.data.isForwardPtr == 0);

  const uint8_t prims = header.data.prims;
  const uint8_t ptrs = header.data.ptrs;
  const word obj_size = 1+prims+ptrs;

  if( header.data.gen == 0 ) {
    header.data.black = semi->black_bit;
    header.data.gen = 1;
    ns->evacuated += obj_size;

    *obj = header.raw;

    switch(ptrs) {
      case 0: return;
      case 1: return nursery_evacuate_bypass(ns, semi, (hp*) obj+1+prims);
      case 2:
        nursery_evacuate_bypass(ns, semi, (hp*) obj+1+prims);
        return nursery_evacuate_bypass(ns, semi, (hp*) obj+1+prims+1);
      default:
        for(int i=0;i<ptrs;i++) {
          nursery_evacuate_bypass(ns, semi, (hp*) obj+1+prims+i);
        }
        return;
    }
  } else if( header.data.gen == 1 ) {

    // obj is in semi-space gen
    if( !IS_WHITE(semi, header) ) return;

    const hp dst = semi_bump_grey(semi, obj_size);
    header.data.grey = true;
    header.data.black = !semi->black_bit;

    *objAddr = dst;
    writeIndirection(obj, dst);

    *dst = header.raw;
    #pragma clang loop unroll_count(1)
    for(int i=1;i<obj_size;i++) {
      dst[i] = obj[i];
    }
  } else {
    __builtin_unreachable();
    // abort();
  }
}

void nursery_bypass(Nursery *ns, SemiSpace *semi) {
  hp free = semi_bump_black(semi, NURSERY_SIZE);
  ns->index = free;
  ns->limit = free+NURSERY_SIZE;
  ns->bypass = true;
}

bool nursery_member(Nursery *ns, hp obj) {
  // return obj >= ns->heap && obj < ns->heap+NURSERY_SIZE;
  return obj >= ns->heap && obj < ns->limit;
}
