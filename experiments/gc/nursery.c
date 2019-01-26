#include "common.h"
#include "objects.h"
#include "nursery.h"
#include "header.h"
#include "semispace.h"
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdbool.h>

void nursery_init(Nursery *ns) {
  memset(ns->heap, 0, sizeof(ns->heap));
  ns->index=ns->heap;
  ns->evacuated=0;
}

hp _allocate(Nursery *ns, enum Tag t, Object o, unsigned long size) {
  hp ret = ns->index;
  Header header;

  if(ns->heap+NURSERY_SIZE < ns->index+(size/sizeof(word)+1))
    return NULL;

  header.raw = 0;
  header.data.tag = t;
  *ns->index = header.raw;
  ns->index++;
  memcpy(ns->index, &o, size);
  ns->index += size/sizeof(word);
  return ret;
}

Header readHeader(hp ptr) {
  return *(Header*)ptr;
}
Object* readObject(hp ptr) {
  return (Object*)(ptr+1);
}

void writeIndirection(hp from, hp to) {
  Header ind;
  ind.forwardPtr = to;
  ind.data.isForwardPtr = 1;
  *from = ind.raw;
}

// Depth-first evacuation of objects from nursery to semi-space.
// Semi-space is garanteed to have enough space. No need to check.
// Objects from nursery are moved to black area and immediately
// scavenged. Reachable white objects from semi-space are evacuated
// to grey space and not scavenged.
void nursery_evacuate(Nursery *ns, SemiSpace *semi, hp* objAddr) {
  hp obj = *objAddr, dst;
  Header header;
  word obj_size;
  header = readHeader(obj);
  while( header.data.isForwardPtr ) {
    obj = (hp) ((word)header.forwardPtr & (~1));
    *objAddr = obj;
    header = readHeader(obj);
  }
  assert( header.data.isForwardPtr == 0);

  obj_size = 1+InfoTable[header.data.tag].prims+InfoTable[header.data.tag].ptrs;

  if( header.data.gen == 0 ) {
    // obj is in nursery
    // printf("Evacuate nursery object: %d\n", header.data.tag);
    dst = semi_bump_black(semi, obj_size);
    header.data.black = semi->black_bit;
    ns->evacuated++;
  } else if( header.data.gen == 1 ) {
    // obj is in semi-space gen
    if( header.data.black == semi->black_bit ) {
      // printf("Evacuate black object: %d\n", header.data.tag);
      return;
    }
    if( header.data.grey ) {
      return;
    }
    // printf("Evacuate white object: %d\n", header.data.tag);
    dst = semi_bump_grey(semi, obj_size);
    header.data.grey = true;
    header.data.black = !semi->black_bit;
  } else {
    abort();
  }
  header.data.gen = 1;

  *objAddr = dst;
  writeIndirection(obj, dst);

  *dst = header.raw;
  for(int i=1;i<obj_size;i++) {
    dst[i] = obj[i];
  }

  if( header.data.black == semi->black_bit ) {
    for(int i=0;i<InfoTable[header.data.tag].ptrs;i++) {
      hp child;
      child = dst+1+InfoTable[header.data.tag].prims+i;
      nursery_evacuate(ns, semi, (hp*) child);
    }
  }

  // if obj is in nursery:
  //   write indirection
  //   set header color to black
  //   copy header to semi (black area)
  //   copy body
  //   scavenge object
  // if not:
  //   write indirection
  //   set header color to grey
  //   copy header to semi (grey area)
  //   copy body
}

void nursery_reset(Nursery *ns) {
  memset(ns->heap, 0, NURSERY_SIZE*sizeof(word));
  ns->index = ns->heap;
  ns->evacuated=0;
}

bool nursery_member(Nursery *ns, hp obj) {
  return obj >= ns->heap && obj < ns->heap+NURSERY_SIZE;
}
