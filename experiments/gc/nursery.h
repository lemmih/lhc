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
// #define NURSERY_SIZE (1024*256/sizeof(word))

typedef struct {
  hp index;
  hp limit;
  unsigned int evacuated;
  bool bypass;
  word heap[NURSERY_SIZE];
} Nursery;

void nursery_init(Nursery *);

#define allocate(ns, semi, tag, ...) _allocate(ns, semi, tag, InfoTable[tag].prims, InfoTable[tag].ptrs, ((Object)(__VA_ARGS__)), sizeof((__VA_ARGS__)))
// hp _allocate(Nursery *ns, enum Tag t, Object o, unsigned long size);

void nursery_evacuate(Nursery *ns, SemiSpace *semi, hp* objAddr);
void nursery_reset(Nursery *ns, SemiSpace *semi, Stats *);
void nursery_bypass(Nursery *ns, SemiSpace *semi);
bool nursery_member(Nursery *ns, hp obj);

static hp _allocate(Nursery *ns, SemiSpace *semi, enum Tag t, uint8_t prims, uint8_t ptrs, Object o, unsigned long size) {
  hp ret = ns->index;
  Header header;

  if(ns->limit < ns->index+(size/sizeof(word)+1))
    return NULL;

  header.raw = 0;
  header.data.tag = t;
  header.data.prims = prims;
  header.data.ptrs = ptrs;
  *ns->index = header.raw;
  ns->index++;
  memcpy(ns->index, &o, size);
  ns->index += size/sizeof(word);

  // if(ns->bypass) {
  //   nursery_evacuate(ns, semi, &ret);
  // }
  return ret;
}


#endif
