#include "api.h"

#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <sys/param.h>

word *_lhc_semi_init();
void _lhc_semi_begin();
word *_lhc_semi_end();
word *_lhc_semi_mark(word *object);
word *_lhc_semi_mark_frame(word *object);
word _lhc_semi_allocate(word*, word);
void evacuate_frame(word **object_address);
void evacuate(word **object_address);
void scavenge_records();
void scavenge();

#define MIN_HEAP (1024*128)
static int size=MIN_HEAP;
static int live;
static int factor=2;

static word *hp;
static word *hp_limit;

static word *from_space;
static word *to_space;
static word *scavenged;

word _lhc_semi_allocate(word *hp, word space) {
  return hp+space<hp_limit;
}

word* _lhc_semi_init() {
  hp = malloc(size*sizeof(word));
  from_space = hp;
  hp_limit = hp+size;
  to_space = NULL;
  return hp;
}

void _lhc_semi_begin() {
  assert(to_space==NULL);
  assert(size>0);
  to_space = malloc(size*sizeof(word));
  scavenged = to_space;
  live = 0;
}
word *_lhc_semi_end() {
  // All roots have been marked.
  // Start by scavenging the frame stack.
  // Then scavenge the rest of the objects.
  // Then free from_space and swap.
  scavenge_records();
  scavenge();
  assert(to_space==scavenged);
  assert(from_space!=NULL);
  free(from_space);
  assert(to_space!=NULL);
  size = MAX(MIN_HEAP,live*factor);
  to_space = NULL;
  scavenged = NULL;
  hp = malloc(size*sizeof(word));
  from_space = hp;
  hp_limit = hp+size;
  return hp;
}

word *_lhc_semi_mark_frame(word *object) {
  if(object==NULL) return NULL;
  evacuate_frame(&object);
  return object;
}
void evacuate_frame(word **object_address) {
  word *object = *object_address;
  word *dst = to_space;
  ActivationInfo *info;
  if(!object) return;
  info=*(ActivationInfo**)object;
  info--;
  memcpy(dst, object, info->recordSize*sizeof(word));
  *object_address = to_space;
  live += info->recordSize;
  to_space+=info->recordSize;

  evacuate_frame((word**)&dst[1]);
}

void scavenge_records() {
  ActivationInfo *info;
  word *next=scavenged;
  while(next==scavenged) {
    info = *(ActivationInfo**)scavenged;
    info--;
    next = (void*)scavenged[1];

    for(int i=0;i<info->nHeapPointers;i++) {
      evacuate((word**)&scavenged[2+info->nPrimitives+i]);
    }
    next = *(word**)&scavenged[1];
    scavenged+=info->recordSize;
  }
}

word *_lhc_semi_mark(word *object) {
  assert(object!=NULL);
  evacuate(&object);
  return object;
}
void evacuate(word **object_address) {
  InfoTable *table;
  size_t size;
  word *object = *object_address;
  // if(!IN_FROM_SPACE(object)) return;
  // if(IS_INDIRECTION(object)) {
  //   *object_address = INDIRECTION(object);
  //   *object = *object_address;
  // }
  table = &_lhc_info_tables[*object];
  size = (1+table->nPrimitives+table->nHeapPointers);

  memcpy(to_space, object, size*sizeof(word));
  *object_address = to_space;
  live += size;
  to_space+=size;
}
void scavenge() {
  InfoTable *table;
  while(scavenged < to_space) {
    table = &_lhc_info_tables[*scavenged];
    scavenged += 1+table->nPrimitives;
    for(int i=0;i<table->nHeapPointers;i++) {
      evacuate((word**)scavenged);
      scavenged++;
    }
  }
}
