#include "semispace.h"
#include "header.h"
#include "objects.h"
#include "nursery.h"
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>

void semi_init(SemiSpace *semi) {
  semi->black_bit=0;
  semi->factor = 1;
  semi->white_space.ptr1 = NULL;
  semi->white_space.ptr2 = NULL;
  semi->white_space.size = 0;
  semi->grey_space.ptr = NULL;
  semi->grey_space.free = NULL;
  semi->grey_space.scavenged = NULL;
  semi->grey_space.size = 0;
  semi->black_space.ptr = calloc(sizeof(word), MIN_AREA);
  semi->black_space.free = semi->black_space.ptr;
  semi->black_space.size = MIN_AREA;
}

void semi_close(SemiSpace *semi) {
  free(semi->white_space.ptr1);
  free(semi->white_space.ptr2);
  free(semi->grey_space.ptr);
  free(semi->black_space.ptr);
  semi->black_bit=0;
  semi->white_space.ptr1 = NULL;
  semi->white_space.ptr2 = NULL;
  semi->white_space.size = 0;
  semi->grey_space.ptr = NULL;
  semi->grey_space.free = NULL;
  semi->grey_space.scavenged = NULL;
  semi->grey_space.size = 0;
  semi->black_space.ptr = NULL;
  semi->black_space.free = NULL;
  semi->black_space.size = 0;
}

// When a nursery is full, all objects in the nursery should be copied to the
// black space. These objects may point to white objects. The white objects
// must be evacuated (copied to the grey space).

hp semi_bump_grey(SemiSpace *semi, word size) {
  hp ret;
  assert(semi->grey_space.free+size <= area_limit(semi->grey_space));
  ret = semi->grey_space.free;
  semi->grey_space.free += size;
  return ret;
}

hp semi_bump_black(SemiSpace *semi, word size) {
  hp ret;
  assert(semi->black_space.free+size <= area_limit(semi->black_space));
  ret = semi->black_space.free;
  semi->black_space.free += size;
  return ret;
}

bool semi_check(SemiSpace *semi, word size) {
  // printf("Semi check: %lu %lu %d\n", area_used(semi->black_space), size, semi->black_space.size);
  return semi->black_space.free+size <= area_limit(semi->black_space);
}

static void writeIndirection(hp from, hp to) {
  Header ind;
  ind.forwardPtr = to;
  ind.data.isForwardPtr = 1;
  *from = ind.raw;
}

void semi_evacuate(SemiSpace *semi, hp* objAddr) {
  hp obj = *objAddr, dst;
  Header header;
  const ObjectInfo *info;
  word obj_size;

  header = readHeader(obj);
  while( header.data.isForwardPtr ) {
    obj = (hp) ((word)header.forwardPtr & (~1));
    *objAddr = obj;
    header = readHeader(obj);
  }
  assert( header.data.isForwardPtr == 0 );

  info = &InfoTable[header.data.tag];
  obj_size = 1+info->prims+info->ptrs;

  assert( header.data.gen == 1 );

  if( header.data.black == semi->black_bit ) {
    return;
  }
  if( header.data.grey ) {
    return;
  }

  dst = semi_bump_grey(semi, obj_size);
  header.data.grey = true;
  header.data.black = !semi->black_bit;

  *objAddr = dst;
  writeIndirection(obj, dst);

  *dst = header.raw;
  for(int i=1;i<obj_size;i++) {
    dst[i] = obj[i];
  }
}

void semi_scavenge(SemiSpace *semi) {
  Header header;
  const ObjectInfo *info;
  while(semi->grey_space.scavenged<semi->grey_space.free) {
    header = readHeader(semi->grey_space.scavenged);
    assert(header.data.grey == true);
    header.data.grey = false;
    header.data.black = semi->black_bit;
    *semi->grey_space.scavenged = header.raw;

    info = &InfoTable[header.data.tag];
    semi->grey_space.scavenged += 1 + info->prims;
    for(int i=0;i<info->ptrs;i++) {
      semi_evacuate(semi, (hp*) semi->grey_space.scavenged);
      semi->grey_space.scavenged++;
    }
  }
  // All white objects are now dead and all objects in "grey" area have been
  // marked black. Deallocate "white" area, combine "grey" and "black" area into
  // new white area. Flip black bit.
  semi->black_bit = !semi->black_bit;
  free(semi->white_space.ptr1);
  free(semi->white_space.ptr2);
  semi->white_space.ptr1 = semi->grey_space.ptr;
  semi->white_space.ptr2 = semi->black_space.ptr;
  semi->white_space.size = area_used(semi->grey_space) + area_used(semi->black_space);

  semi->grey_space.size = semi->white_space.size;
  semi->grey_space.ptr = calloc(sizeof(word), semi->grey_space.size);
  semi->grey_space.free = semi->grey_space.ptr;
  semi->grey_space.scavenged = semi->grey_space.ptr;

  semi->black_space.size = MAX(MIN_AREA,semi->white_space.size * semi->factor);
  semi->black_space.ptr = calloc(sizeof(word), semi->black_space.size);
  semi->black_space.free = semi->black_space.ptr;

  assert(area_used(semi->grey_space)==0);
  assert(area_used(semi->black_space)==0);
}

word semi_size(SemiSpace *semi) {
  return semi->white_space.size + area_used(semi->grey_space) +
         area_used(semi->black_space);
}
