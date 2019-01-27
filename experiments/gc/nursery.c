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

static void nursery_evacuate_plain(Nursery *ns, SemiSpace *semi, hp* objAddr);
static void nursery_evacuate_bypass(Nursery *ns, SemiSpace *semi, hp* objAddr);
static void nursery_evacuate_plain_stack(Nursery *ns, SemiSpace *semi, hp* stack[], int idx);

void nursery_init(Nursery *ns) {
  memset(ns->heap, 0, sizeof(ns->heap));
  touchSpace(ns->heap, NURSERY_SIZE);
  ns->index=ns->heap;
  ns->limit=ns->heap+NURSERY_SIZE;
  ns->evacuated=0;
  ns->bypass = false;
}


// Depth-first evacuation of objects from nursery to semi-space.
// Semi-space is garanteed to have enough space. No need to check.
// Objects from nursery are moved to black area and immediately
// scavenged. Reachable white objects from semi-space are evacuated
// to grey space and not scavenged.
void nursery_evacuate(Nursery *ns, SemiSpace *semi, hp* objAddr) {
  hp* stack[NURSERY_SIZE];
  if(ns->bypass) {
    return nursery_evacuate_bypass(ns, semi, objAddr);
  } else {
    return nursery_evacuate_plain(ns, semi, objAddr);
    // stack[0] = objAddr;
    // nursery_evacuate_plain_stack(ns, semi, stack, 1);
    // return;
  }
}

// static void nursery_evacuate_plain_stack(Nursery *ns, SemiSpace *semi, hp* stack[], int idx) {
//   hp obj, dst;
//   Header header;
//   word obj_size;
//   uint8_t prims, ptrs;
//   // printf("Begin evacuate\n");
//   while(idx) {
//     // printf("idx: %d\n", idx);
//     hp* objAddr = stack[--idx];
//     obj = *objAddr;
//     header = readHeader(obj);
//     while( header.data.isForwardPtr ) {
//       // printf("Found indirection\n");
//       obj = (hp) ((word)header.forwardPtr & (~1));
//       *objAddr = obj;
//       header = readHeader(obj);
//     }
//     assert( header.data.isForwardPtr == 0);
//
//     prims = InfoTable[header.data.tag].prims;
//     ptrs = InfoTable[header.data.tag].ptrs;
//     obj_size = 1+prims+ptrs;
//
//     switch( header.data.gen ) {
//       case 0:
//         // printf("Found nursery object: %d\n", header.data.tag);
//         dst = semi_bump_black(semi, obj_size);
//         header.data.black = semi->black_bit;
//         header.data.gen = 1;
//         ns->evacuated += obj_size;
//
//         *objAddr = dst;
//         writeIndirection(obj, dst);
//
//         *dst = header.raw;
//         for(int i=1;i<obj_size;i++) {
//           dst[i] = obj[i];
//         }
//
//         for(int i=0;i<ptrs;i++) {
//           stack[idx++] = (hp*) dst+1+prims+i;
//         }
//         continue;
//       case 1:
//         // printf("Found gen1 object: %d\n", header.data.tag);
//         if( !IS_WHITE(semi, header) ) continue;
//
//         dst = semi_bump_grey(semi, obj_size);
//         header.data.grey = true;
//         header.data.black = !semi->black_bit;
//
//         *objAddr = dst;
//         writeIndirection(obj, dst);
//
//         *dst = header.raw;
//         for(int i=1;i<obj_size;i++) {
//           dst[i] = obj[i];
//         }
//         continue;
//       default:
//         abort();
//     }
//   }
//   // printf("End evacuate\n");
// }

static void nursery_evacuate_plain(Nursery *ns, SemiSpace *semi, hp* objAddr) {
  hp obj = *objAddr, dst;
  Header header;
  word obj_size;
  uint8_t prims, ptrs;

  header = readHeader(obj);
  while( header.data.isForwardPtr ) {
    obj = (hp) ((word)header.forwardPtr & (~1));
    *objAddr = obj;
    header = readHeader(obj);
  }
  assert( header.data.isForwardPtr == 0);

  prims = InfoTable[header.data.tag].prims;
  ptrs = InfoTable[header.data.tag].ptrs;
  obj_size = 1+prims+ptrs;

  switch( header.data.gen ) {
    case 0:
      dst = semi_bump_black(semi, obj_size);
      header.data.black = semi->black_bit;
      header.data.gen = 1;
      ns->evacuated += obj_size;

      *objAddr = dst;
      writeIndirection(obj, dst);

      *dst = header.raw;
      for(int i=1;i<obj_size;i++) {
        dst[i] = obj[i];
      }

      switch(ptrs) {
        case 0: return;
        case 1: return nursery_evacuate_plain(ns, semi, (hp*) dst+1+prims);
        case 2:
          nursery_evacuate_plain(ns, semi, (hp*) dst+1+prims);
          return nursery_evacuate_plain(ns, semi, (hp*) dst+1+prims+1);
        default:
          for(int i=0;i<ptrs;i++)
            nursery_evacuate_plain(ns, semi, (hp*) dst+1+prims+i);
          return;
      }
    case 1:
      if( !IS_WHITE(semi, header) ) return;

      dst = semi_bump_grey(semi, obj_size);
      header.data.grey = true;
      header.data.black = !semi->black_bit;

      *objAddr = dst;
      writeIndirection(obj, dst);

      *dst = header.raw;
      for(int i=1;i<obj_size;i++) {
        dst[i] = obj[i];
      }
      return;
    default:
      abort();
  }
}

static void nursery_evacuate_bypass(Nursery *ns, SemiSpace *semi, hp* objAddr) {
  hp obj = *objAddr;
  Header header;
  word obj_size;
  uint8_t prims, ptrs;

  header = readHeader(obj);
  while( header.data.isForwardPtr ) {
    obj = (hp) ((word)header.forwardPtr & (~1));
    *objAddr = obj;
    header = readHeader(obj);
  }
  assert( header.data.isForwardPtr == 0);

  prims = InfoTable[header.data.tag].prims;
  ptrs = InfoTable[header.data.tag].ptrs;
  obj_size = 1+prims+ptrs;

  if( header.data.gen == 0 ) {
    header.data.black = semi->black_bit;
    header.data.gen = 1;
    ns->evacuated += obj_size;

    *obj = header.raw;

    if(InfoTable[header.data.tag].ptrs==1) {
      return nursery_evacuate_bypass(ns, semi, (hp*) obj+1+prims);
    } else {
      for(int i=0;i<ptrs;i++) {
        nursery_evacuate_bypass(ns, semi, (hp*) obj+1+prims+i);
      }
    }
  } else if( header.data.gen == 1 ) {
    hp dst;
    // obj is in semi-space gen
    if( !IS_WHITE(semi, header) ) return;

    dst = semi_bump_grey(semi, obj_size);
    header.data.grey = true;
    header.data.black = !semi->black_bit;

    *objAddr = dst;
    writeIndirection(obj, dst);

    *dst = header.raw;
    for(int i=1;i<obj_size;i++) {
      dst[i] = obj[i];
    }
  } else {
    abort();
  }
}

void nursery_reset(Nursery *ns, Stats *s) {
  s->nursery_n_collections++;
  s->allocated += NURSERY_SIZE - (ns->limit - ns->index);
  s->nursery_copied += ns->evacuated;
  ns->index = ns->heap;
  ns->limit = ns->heap+NURSERY_SIZE;
  ns->evacuated=0;
  ns->bypass = false;
}

void nursery_bypass(Nursery *ns, SemiSpace *semi) {
  hp free = semi_bump_black(semi, NURSERY_SIZE);
  ns->index = free;
  ns->limit = free+NURSERY_SIZE;
  ns->bypass = true;
}

bool nursery_member(Nursery *ns, hp obj) {
  return obj >= ns->heap && obj < ns->heap+NURSERY_SIZE;
}
