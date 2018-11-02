#include "api.h"
#include "stats.h"

#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <sys/param.h>

int _lhc_enable_tail_copying = 0;
int _lhc_enable_padding = 0;

word *_lhc_semi_init();
void _lhc_semi_begin();
word *_lhc_semi_end();
word *_lhc_semi_mark(word *object);
word *_lhc_semi_mark_frame(word *object);
word _lhc_semi_allocate(word*, word);
static void evacuate_frame(word **object_address);
static void evacuate(word*, word **);
static void scavenge_records();
static void scavenge();

#define MIN_HEAP (1024*128)
#define MAX_HEAP (1024*1024*128*8)
static int size=MIN_HEAP;
static int live;
static int factor=3;

static word *hp_limit;

static word *from_space;
static word *to_space, *free_space, *prev_heap;
static word *scavenged;

word _lhc_semi_allocate(word *hp, word space) {
  return hp+space<hp_limit;
}

word* _lhc_semi_init() {
  word *hp;
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

void _lhc_semi_begin() {
  _lhc_stats_start_gc();
  _lhc_stats_collect();
  assert(to_space==NULL);
  assert(free_space==NULL);
  assert(size>0);
  _lhc_stats_allocate(size*sizeof(word));
  to_space = malloc(size * sizeof(word));
  //printf("SemiSpace begin, to_space: %p\n", to_space);
  free_space = to_space;
  scavenged = to_space;
  live = 0;
}
word *_lhc_semi_end() {
  word *hp;
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
  hp = malloc(size * sizeof(word));
  assert(hp!=NULL);
  from_space = hp;
  hp_limit = hp+size;
  // printf("SemiSpace done. Live: %d, size: %d, allocated: %lu\n", live, size, _lhc_stats_allocated);
  _lhc_stats_live(live*sizeof(word));
  _lhc_stats_copy(live*sizeof(word));
  _lhc_stats_heap(size*sizeof(word));
  _lhc_stats_end_gc();
  return hp;
}

word *_lhc_semi_mark_frame(word *object) {
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
      evacuate(NULL, (word**)&scavenged[2+info->nPrimitives+i]);
      new_addr = *(word**)&scavenged[2+info->nPrimitives+i];
      // printf(" %lu", new_addr-to_space);
    }
    // printf("\n");
    next = *(word**)&scavenged[1];
    scavenged+=info->recordSize;
  }
}

word *_lhc_semi_mark(word *object) {
  assert(object!=NULL);
  evacuate(NULL, &object);
  assert(object >= to_space && object < free_space);
  return object;
}

// // evacuate an object with at least 1 tail object.
// static void evacuate1(word *parent, word **object_address) {
//   const InfoTable *table;
//   size_t size;
//   word *object = *object_address;
//   word header=*object;
//   while(_lhc_isIndirection(header)) {
//     object = _lhc_getIndirection(header);
//     *object_address = object;
//     header=*object;
//   }
//   if(object >= to_space && object < free_space) {
//     return;
//   }
//   table = &_lhc_info_tables[_lhc_getTag(header)];
//   size = (1+table->nPrimitives+table->nHeapPointers);
//
//   *parent = _lhc_incSize(*parent, 1);
//   free_space[0] = header;
//   #pragma clang loop unroll_count(3)
//   for(int i=1; i<size; i++)
//     free_space[i] = object[i];
//   _lhc_setIndirection(object, free_space);
//   *object_address = free_space;
//   live += size;
//   free_space+=size;
//
//   word **tail_pointer = (word**)(free_space-1);
//   return evacuate(parent, tail_pointer);
// }
//
// // evacuate an object with at least 2 tail objects.
// static void evacuate2(word *parent, word **object_address) {
//   const InfoTable *table;
//   size_t size;
//   word *object = *object_address;
//   word header=*object;
//   while(_lhc_isIndirection(header)) {
//     object = _lhc_getIndirection(header);
//     *object_address = object;
//     header=*object;
//   }
//   if(object >= to_space && object < free_space) {
//     return;
//   }
//   table = &_lhc_info_tables[_lhc_getTag(header)];
//   size = (1+table->nPrimitives+table->nHeapPointers);
//
//   *parent = _lhc_incSize(*parent, 1);
//   free_space[0] = header;
//   #pragma clang loop unroll_count(3)
//   for(int i=1; i<size; i++)
//     free_space[i] = object[i];
//   _lhc_setIndirection(object, free_space);
//   *object_address = free_space;
//   live += size;
//   free_space+=size;
//
//   word **tail_pointer = (word**)(free_space-1);
//   return evacuate1(parent, tail_pointer);
// }
//
// // evacuate an object with at least 3 tail objects.
// static void evacuate3(word *parent, word **object_address) {
//   const InfoTable *table;
//   size_t size;
//   word *object = *object_address;
//   word header=*object;
//   while(_lhc_isIndirection(header)) {
//     object = _lhc_getIndirection(header);
//     *object_address = object;
//     header=*object;
//   }
//   if(object >= to_space && object < free_space) {
//     return;
//   }
//   table = &_lhc_info_tables[_lhc_getTag(header)];
//   size = (1+table->nPrimitives+table->nHeapPointers);
//
//   *parent = _lhc_incSize(*parent, 1);
//   free_space[0] = header;
//   #pragma clang loop unroll_count(3)
//   for(int i=1; i<size; i++)
//     free_space[i] = object[i];
//   _lhc_setIndirection(object, free_space);
//   *object_address = free_space;
//   live += size;
//   free_space+=size;
//
//   word **tail_pointer = (word**)(free_space-1);
//   return evacuate2(parent, tail_pointer);
// }
//
// // evacuate an object with at least 4 tail objects.
// static void evacuate4(word *parent, word **object_address) {
//   const InfoTable *table;
//   size_t size;
//   word *object = *object_address;
//   word header=*object;
//   while(_lhc_isIndirection(header)) {
//     object = _lhc_getIndirection(header);
//     *object_address = object;
//     header=*object;
//   }
//   if(object >= to_space && object < free_space) {
//     return;
//   }
//   table = &_lhc_info_tables[_lhc_getTag(header)];
//   size = (1+table->nPrimitives+table->nHeapPointers);
//
//   *parent = _lhc_incSize(*parent, 1);
//   free_space[0] = header;
//   #pragma clang loop unroll_count(3)
//   for(int i=1; i<size; i++)
//     free_space[i] = object[i];
//   _lhc_setIndirection(object, free_space);
//   *object_address = free_space;
//   live += size;
//   free_space+=size;
//
//   word **tail_pointer = (word**)(free_space-1);
//   return evacuate3(parent, tail_pointer);
// }
//
// // evacuate an object with at least 5 tail objects.
// static void evacuate5(word *parent, word **object_address) {
//   const InfoTable *table;
//   size_t size;
//   word *object = *object_address;
//   word header=*object;
//   while(_lhc_isIndirection(header)) {
//     object = _lhc_getIndirection(header);
//     *object_address = object;
//     header=*object;
//   }
//   if(object >= to_space && object < free_space) {
//     return;
//   }
//   table = &_lhc_info_tables[_lhc_getTag(header)];
//   size = (1+table->nPrimitives+table->nHeapPointers);
//
//   *parent = _lhc_incSize(*parent, 1);
//   free_space[0] = header;
//   #pragma clang loop unroll_count(3)
//   for(int i=1; i<size; i++)
//     free_space[i] = object[i];
//   _lhc_setIndirection(object, free_space);
//   *object_address = free_space;
//   live += size;
//   free_space+=size;
//
//   word **tail_pointer = (word**)(free_space-1);
//   return evacuate4(parent, tail_pointer);
// }

static void evacuate(word *parent, word **object_address) {
  const InfoTable *table;
  size_t size;
  word *object = *object_address;
  word header=*object;
  word nTailObjs = 0;
  while(_lhc_isIndirection(header)) {
    object = _lhc_getIndirection(header);
    *object_address = object;
    header=*object;
  }
  if(object >= to_space && object < free_space) {
    // printf("Already evacuated object.\n");
    return;
  }
  table = &_lhc_info_tables[_lhc_getTag(header)];
  size = (1+table->nPrimitives+table->nHeapPointers);
  if(_lhc_enable_padding && table->nHeapPointers)
    size++;
  // if(!_lhc_enable_padding && table->nHeapPointers)
  //   live++;

  // if(_lhc_enable_tail_copying && table->nHeapPointers) {
  //   if(parent) {
  //     *parent = _lhc_incSize(*parent, 1);
  //     nTailObjs = _lhc_getSize(header);
  //     nTailObjs = nTailObjs ? nTailObjs : 1;
  //     header = _lhc_setSize(header, 0);
  //   } else {
  //     parent = free_space;
  //     nTailObjs = _lhc_getSize(header);
  //     nTailObjs = nTailObjs ? nTailObjs : 1;
  //     header = _lhc_setSize(header, 0);
  //   }
  // }
  _lhc_setIndirection(object, free_space);
  *object_address = free_space;
  live += size;
  free_space[0] = header;
  #pragma clang loop unroll_count(2)
  for(int i=1; i<size; i++)
    free_space[i] = object[i];
  free_space+=size;

  if(_lhc_enable_tail_copying && table->nHeapPointers) {
    word **tail_pointer = (word**)(free_space-1);
    // if(_lhc_enable_padding)
    //   tail_pointer--;
    return evacuate(parent, tail_pointer);
  }
  // _lhc_stats_evacuation();
  // if(table->nHeapPointers) {
  //   // _lhc_stats_tail_evacuation();
  //   if(_lhc_enable_tail_copying) {
  //     word **tail_pointer = (word**)(free_space-1);
  //     return evacuate(parent, tail_pointer);
  //   }
  // }
}


// static void scavenge() {
//   InfoTable *table;
//   while(scavenged < free_space) {
//     word* obj = scavenged;
//     table = &_lhc_info_tables[_lhc_getTag(*scavenged)];
//     // printf("SemiSpace: Scavenge: %ld = %ld", scavenged-to_space, *scavenged);
//     for(int i=0;i<table->nPrimitives;i++) {
//       // printf(" (%lu)", *(scavenged+1+i));
//     }
//     scavenged += 1+table->nPrimitives;
//     for(int i=0;i<table->nHeapPointers;i++) {
//       if(_lhc_enable_tail_copying && i == table->nHeapPointers-1) {
//         scavenged++;
//         break;
//       }
//       evacuate((word**)scavenged);
//       scavenged++;
//     }
//     // printf("\n");
//     _lhc_stats_scavenged(obj);
//   }
// }
static void scavenge() {
  const InfoTable *table;
  word* s = scavenged;
  if(_lhc_enable_tail_copying) {
    while(s < free_space) {
      word* obj = s;
      table = &_lhc_info_tables[_lhc_getTag(*s)];
      s += 1+table->nPrimitives;
      // #pragma clang loop unroll_count(8)
      for(int i=1;i<table->nHeapPointers;i++) {
        evacuate(NULL, (word**)s);
        s++;
      }
      if(table->nHeapPointers) s++;
      if(_lhc_enable_padding && table->nHeapPointers)
        s++;
      // _lhc_stats_scavenged(obj);
    }
    scavenged = s;
  } else {
    while(scavenged < free_space) {
      word* obj = scavenged;
      table = &_lhc_info_tables[_lhc_getTag(*scavenged)];
      scavenged += 1+table->nPrimitives;
      // #pragma clang loop unroll_count(8)
      for(int i=0;i<table->nHeapPointers;i++) {
        evacuate(NULL, (word**)scavenged);
        scavenged++;
      }
      if(_lhc_enable_padding && table->nHeapPointers)
        scavenged++;

      // _lhc_stats_scavenged(obj);
    }
  }
}
