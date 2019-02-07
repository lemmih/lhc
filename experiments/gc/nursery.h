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

#define NURSERY_SIZE (1024*512/sizeof(word))
// #define NURSERY_SIZE (1024*1024/sizeof(word))

typedef struct {
  hp index;
  hp limit;
  word evacuated;
  bool bypass;
  unsigned int roots;
  hp scavenged;
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


#define allocate(ns, semi, tag, ...) _allocate(ns, semi, tag, InfoTable[tag].prims, InfoTable[tag].ptrs, ((Object)(__VA_ARGS__)), sizeof((__VA_ARGS__)))
// hp _allocate(Nursery *ns, enum Tag t, Object o, unsigned long size);

void nursery_begin(Nursery *ns, SemiSpace *semi, Stats *s);
void nursery_evacuate(Nursery *ns, SemiSpace *semi, hp* objAddr);
// void nursery_reset(Nursery *ns, SemiSpace *semi, Stats *);
void nursery_bypass(Nursery *ns, SemiSpace *semi);
bool nursery_member(Nursery *ns, hp obj);
void nursery_scavenge(Nursery *ns, SemiSpace *semi);

static hp _allocate(Nursery *ns, SemiSpace *semi, enum Tag t, uint8_t prims, uint8_t ptrs, Object o, unsigned long size) {
  Header header;

  assert(prims+ptrs == size/sizeof(word));

  __builtin_assume(ns->index != NULL);
  __builtin_assume(ns->limit != NULL);

  if(ns->limit < ns->index+(size/sizeof(word)+1))
    return NULL;

  const hp ret = ns->index;

  header.raw = 0;
  header.data.tag = t;
  header.data.prims = prims;
  header.data.ptrs = ptrs;
  *ns->index = header.raw;
  ns->index++;
  memcpy(ns->index, &o, size);
  ns->index += size/sizeof(word);

  __builtin_assume(ret != NULL);

  // if(ns->bypass) {
  //   nursery_evacuate(ns, semi, &ret);
  // }
  return ret;
}
static void nursery_end(Nursery *ns, SemiSpace *semi, Stats *s) {
  // printf("NS: %u %u %u%%\n", ns->evacuated, ns->roots, ns->roots*100/ns->evacuated);

  nursery_scavenge(ns, semi);

  s->nursery_n_collections++;
  s->allocated += NURSERY_SIZE - (ns->limit - ns->index);
  s->nursery_copied += ns->evacuated;
  ns->evacuated=0;
  semi->has_roots = true;
  ns->limit = ns->heap+NURSERY_SIZE;
  ns->bypass = false;
  ns->roots = 0;
  ns->index = ns->heap;


  stats_timer_end(s);

  // semi_scavenge_concurrent(semi, s);

}


#endif
