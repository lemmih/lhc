#include "api.h"
#include "stats.h"

#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <sys/param.h>

word *_lhc_cached_init();
void _lhc_cached_begin();
word *_lhc_cached_end();
word *_lhc_cached_mark(word *object);
word *_lhc_cached_mark_frame(word *object);
word _lhc_cached_allocate(word*, word);
static void evacuate_frame(word **object_address);
static void evacuate(word **object_address);
static void scavenge_records();
static void scavenge();

#define MIN_HEAP (1024*128)
#define MAX_HEAP (1024*1024*128)
static int size=MIN_HEAP;
static int live;
static int factor=3;

static word *hp_limit;

static word *from_space;
static word *to_space, *free_space, *prev_heap;
static word *scavenged;

word _lhc_cached_allocate(word *hp, word space) {
  // printf("SemiSpace alloc: %p %ld\n", hp, space);
  _lhc_stats_allocate(space*sizeof(word));
  return hp+space<hp_limit;
}

word* _lhc_cached_init() {
  void *hp;
  assert  (sizeof(word)==8); // 64bit only for now

  hp = calloc(size,sizeof(word));
  from_space = hp;
  hp_limit = hp+size;
  to_space = NULL;
  prev_heap = NULL;
  free_space = NULL;
  // printf("SemiSpace init: hp: %p, size: %d\n", hp, size);
  _lhc_stats_heap(MIN_HEAP*sizeof(word));
  return hp;
}

void _lhc_cached_begin() {
  _lhc_stats_collect();
  assert(to_space==NULL);
  assert(free_space==NULL);
  assert(size>0);
  to_space = calloc(size,sizeof(word));
  //printf("SemiSpace begin, to_space: %p\n", to_space);
  free_space = to_space;
  scavenged = to_space;
  live = 0;
}
word *_lhc_cached_end() {
  void *hp;
  // All roots have been marked.
  // Start by scavenging the frame stack.
  // Then scavenge the rest of the objects.
  // Then free from_space and swap.
  scavenge_records();
  scavenge();
  assert(free_space==scavenged);
  assert(from_space!=NULL);
  free(from_space);
  if(prev_heap) {
    free(prev_heap);
  }
  assert(to_space!=NULL);
  size = MAX(MIN_HEAP,live*factor);
  if(size >= MAX_HEAP) {
    printf("Out of heap\n");
    abort();
  }
  prev_heap = to_space;
  to_space = NULL;
  free_space = NULL;
  scavenged = NULL;
  hp = calloc(size,sizeof(word));
  assert(hp!=NULL);
  from_space = hp;
  hp_limit = hp+size;
  // printf("SemiSpace done. Live: %d, size: %d\n", live, size);
  _lhc_stats_live(live*sizeof(word));
  _lhc_stats_heap(size*sizeof(word));
  return hp;
}

word *_lhc_cached_mark_frame(word *object) {
  word *orig_object = object;
  if(object==NULL) return NULL;
  evacuate_frame(&object);
  // printf("Evacuate: %p %p %p %p\n", orig_object, object, to_space, free_space);
  assert((object >= to_space && object < free_space) || object==NULL);
  return object;
}
static void evacuate_frame(word **object_address) {
  word *object = *object_address;
  word *dst = free_space;
  ActivationInfo *info;
  if(!object) return;
  info=*(ActivationInfo**)object;
  info--;
  // printf("SemiSpace frame: primitives=%d pointers=%d size=%d\n", info->nPrimitives, info->nHeapPointers, info->recordSize);
  memcpy(dst, object, info->recordSize*sizeof(word));
  *object_address = free_space;
  live += info->recordSize;
  free_space+=info->recordSize;

  evacuate_frame((word**)&dst[1]);
}

static void scavenge_records() {
  ActivationInfo *info;
  word *next=scavenged;
  while(next==scavenged) {
    info = *(ActivationInfo**)scavenged;
    info--;
    // printf("SemiSpace: Scavenge Frame: %ld = %d/%d/%d", scavenged-to_space, info->nPrimitives, info->nHeapPointers, info->recordSize);
    for(int i=0;i<info->nPrimitives;i++) {
      // printf(" (%lu)", *(scavenged+1+i));
    }
    for(int i=0;i<info->nHeapPointers;i++) {
      word *new_addr;
      evacuate((word**)&scavenged[2+info->nPrimitives+i]);
      new_addr = *(word**)&scavenged[2+info->nPrimitives+i];
      // printf(" %lu", new_addr-to_space);
    }
    // printf("\n");
    next = *(word**)&scavenged[1];
    scavenged+=info->recordSize;
  }
}

word *_lhc_cached_mark(word *object) {
  assert(object!=NULL);
  evacuate(&object);
  assert(object >= to_space && object < free_space);
  return object;
}

static void evacuate(word **object_address) {
  InfoTable *table;
  size_t size;
  word *object = *object_address;
  // if(!IN_FROM_SPACE(object)) return;
  while(_lhc_isIndirectionP(object)) {
    *object_address = _lhc_getIndirectionP(object);
    object = *object_address;
  }
  if(object >= to_space && object < free_space) {
    // printf("Already evacuated object.\n");
    return;
  }
  table = &_lhc_info_tables[_lhc_getTag(*object)];
  size = (1+table->nPrimitives+table->nHeapPointers);
  _lhc_stats_copy(size*sizeof(word));

  memcpy(free_space, object, size*sizeof(word));
  _lhc_setIndirection(object, free_space);
  *object_address = free_space;
  live += size;
  free_space+=size;

  if(table->nHeapPointers > 0) {
    word **tail_pointer = (word**)(free_space-1);
    evacuate(tail_pointer);
  }
}
static void scavenge() {
  InfoTable *table;
  while(scavenged < free_space) {
    word* obj = scavenged;
    table = &_lhc_info_tables[_lhc_getTag(*scavenged)];
    // printf("SemiSpace: Scavenge: %ld = %ld", scavenged-to_space, *scavenged);
    for(int i=0;i<table->nPrimitives;i++) {
      // printf(" (%lu)", *(scavenged+1+i));
    }
    scavenged += 1+table->nPrimitives;
    for(int i=0;i<table->nHeapPointers;i++) {
      word *new_addr;
      evacuate((word**)scavenged);
      new_addr = *(word**)scavenged;
      // printf(" %lu", new_addr-to_space);
      scavenged++;
    }
    // printf("\n");
    _lhc_stats_scavenged(obj);
  }
}
