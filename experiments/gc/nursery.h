#ifndef __NURSERY_H__
#define __NURSERY_H__

#include "header.h"
#include "objects.h"
#include "semispace.h"
#include "stats.h"
#include "utils.h"
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdbool.h>

// If undefined, use depth-first search.
// #define BreadthFirstSearch

#define NURSERY_SIZE (1024*512/sizeof(word))
// #define NURSERY_SIZE (1024*1024/sizeof(word))

typedef struct {
  hp index;
  hp limit;
  word evacuated;
  bool bypass;
  unsigned int roots;
  hp black_start;
  word heap[NURSERY_SIZE];
} Nursery;

// void nursery_init(Nursery *);

static void nursery_init(Nursery *ns) {
  memset(ns->heap, 0, sizeof(ns->heap));
  touchSpace(ns->heap, NURSERY_SIZE);
  ns->index=ns->heap;
  ns->limit=ns->heap+NURSERY_SIZE;
  ns->evacuated=0;
  ns->bypass = false;
  ns->roots=0;
}


#define allocate(ns, semi, tag, ...) _allocate(ns, semi, tag, InfoTable[tag].prims, InfoTable[tag].ptrs, InfoTable[tag].mutable, ((Object)(__VA_ARGS__)), sizeof((__VA_ARGS__)))
// hp _allocate(Nursery *ns, enum Tag t, Object o, unsigned long size);

void nursery_begin(Nursery *ns, SemiSpace *semi, Stats *s);
void nursery_evacuate(Nursery *ns, SemiSpace *semi, hp* objAddr);
void nursery_evacuate_one(Nursery *ns, SemiSpace *semi, hp* objAddr);
// void nursery_reset(Nursery *ns, SemiSpace *semi, Stats *);
void nursery_bypass(Nursery *ns, SemiSpace *semi);
bool nursery_member(Nursery *ns, hp obj);
void nursery_scavenge(Nursery *ns, SemiSpace *semi);

static hp _allocate(Nursery *ns, SemiSpace *semi, enum Tag t, uint8_t prims, uint8_t ptrs, bool mutable, Object o, unsigned long size) {
  Header header;

  assert(prims+ptrs == size/sizeof(word));
  assert(prims == InfoTable[t].prims);
  assert(ptrs == InfoTable[t].ptrs);
  assert(mutable == InfoTable[t].mutable);

  if(ns->limit < ns->index+(size/sizeof(word)+1))
    return NULL;

  hp ret = ns->index;
  assert(((word)ret & 1) == ns->bypass);
  // XXX: This has overhead because I can't figure out how to get clang
  //      to put the nursery index and nursery limit in registers.
  //      If everything is correctly inlined and frequently used values put
  //      in registers, the cost of this check is near zero.
  if(((word)ret & 1) == 1) {
    header.raw = 0;
    header.data.tag = t;
    header.data.prims = prims;
    header.data.ptrs = ptrs;
    header.data.mutable = mutable;
    header.data.gen = 1;
    header.data.black = semi->black_bit;
    ret = (hp)((word)ret & ~1);
    *ret = header.raw;
    ns->index++;
    memcpy(&ret[1], &o, size);
    ns->index += size/sizeof(word);
    if(header.data.gen) {
      for(int i=0; i<ptrs; i++) {
        nursery_evacuate_one(ns, semi, (hp*) ret+1+prims+i);
      }
    }
    #ifdef CLANG
    __builtin_assume(ret != NULL);
    #endif
    return ret;
  } else {
    header.raw = 0;
    header.data.tag = t;
    header.data.prims = prims;
    header.data.ptrs = ptrs;
    header.data.mutable = mutable;
    *ns->index = header.raw;
    ns->index++;
    memcpy(ns->index, &o, size);
    ns->index += size/sizeof(word);
    #ifdef CLANG
    __builtin_assume(ret != NULL);
    #endif
    return ret;
  }
}
static void nursery_end(Nursery *ns, SemiSpace *semi, Stats *s) {
  // printf("NS: %u %u %u%%\n", ns->evacuated, ns->roots, ns->roots*100/ns->evacuated);

  #ifdef BreadthFirstSearch
  nursery_scavenge(ns, semi);
  #endif


  s->nursery_n_collections++;
  s->allocated += NURSERY_SIZE - (ns->limit - ns->index);
  s->nursery_copied += semi->black_space.free - ns->black_start;
  semi->has_roots = true;
  ns->limit = ns->heap+NURSERY_SIZE;
  ns->bypass = false;
  ns->roots = 0;
  ns->index = ns->heap;


  stats_timer_end(s);
}


#endif
