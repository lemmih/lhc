#include "semispace.h"
#include "header.h"
#include "objects.h"
#include "nursery.h"
#include "utils.h"
#include "stats.h"
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <sys/mman.h>
#include <assert.h>

void semi_init(SemiSpace *semi) {
  semi->black_bit=0;
  semi->factor = 2;
  semi->has_roots = false;
  semi->white_space.ptr1 = NULL;
  semi->white_space.ptr2 = NULL;
  semi->white_space.size = 0;
  semi->grey_space.ptr = NULL;
  semi->grey_space.free = NULL;
  semi->grey_space.scavenged = NULL;
  semi->grey_space.size = 0;
  semi->black_space.ptr = malloc(sizeof(word) * MIN_AREA);
  semi->black_space.free = semi->black_space.ptr;
  semi->black_space.size = MIN_AREA;

  touchSpace(semi->black_space.ptr, semi->black_space.size);
}

void semi_close(SemiSpace *semi, Stats *s) {
  // stats_misc_begin(s);
  semi_scavenge(semi, s);
  // stats_misc_end(s);
  stats_timer_begin(s, MiscTimer);
  s->gen1_copied += area_used(semi->grey_space);
  const uint64_t heap_size = area_used(semi->grey_space) + area_used(semi->black_space) + semi->white_space.size;
  // printf("Heap: %lu %lu %lu\n", heap_size*8/1024/1024
  //                             , area_used(semi->grey_space)*8/1024/1024
  //                             , area_used(semi->black_space)*8/1024/1024);
  if(s->max_heap < heap_size)
    s->max_heap = heap_size;
  const uint64_t residency = area_used(semi->grey_space) + area_used(semi->black_space);

  // printf("Residency: %lu\n", residency*8/1024/1024);
  if(s->max_residency < residency)
    s->max_residency = residency;

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
  stats_timer_end(s);
}

// When a nursery is full, all objects in the nursery should be copied to the
// black space. These objects may point to white objects. The white objects
// must be evacuated (copied to the grey space).

bool semi_check(SemiSpace *semi, word size) {
  // printf("Semi check: %lu %lu %d\n", area_used(semi->black_space), size, semi->black_space.size);
  return semi->black_space.free+size <= area_limit(semi->black_space);
}


// 0. No grey objects allowed.
// 1. Deallocate white objects.
// 2. Turn black objects white.
void semi_swap_colors(SemiSpace *semi, Stats *s) {
  hp old1, old2;
  assert(semi->grey_space.scavenged == semi->grey_space.free);

  // printf("Gen1 GC done. White: %d, Grey: %ld, Black: %ld\n", semi->white_space.size, area_used(semi->grey_space), area_used(semi->black_space));
  s->gen1_copied += area_used(semi->grey_space);
  // s->gen1_survived += semi->white_space.free;
  // All white objects are now dead and all objects in "grey" area have been
  // marked black. Deallocate "white" area, combine "grey" and "black" area into
  // new white area. Flip black bit.

  const uint64_t heap_size = area_used(semi->grey_space) + area_used(semi->black_space) + semi->white_space.size;
  // printf("Heap: %lu %lu %lu\n", heap_size*8/1024/1024
  //                             , area_used(semi->grey_space)*8/1024/1024
  //                             , area_used(semi->black_space)*8/1024/1024);
  if(s->max_heap < heap_size)
    s->max_heap = heap_size;


  semi->black_bit = !semi->black_bit;
  old1 = semi->white_space.ptr1;
  old2 = semi->white_space.ptr2;
  semi->white_space.ptr1 = semi->grey_space.ptr;
  semi->white_space.ptr2 = semi->black_space.ptr;
  semi->white_space.size = area_used(semi->grey_space) + area_used(semi->black_space);

  // printf("Residency: %d\n", semi->white_space.size*8/1024/1024);
  if(s->max_residency < semi->white_space.size)
    s->max_residency = semi->white_space.size;

  // semi->black_space.size = MAX(MIN_AREA,area_used(semi->grey_space) * semi->factor);
  semi->black_space.size = MAX(MIN_AREA,semi->white_space.size * semi->factor);
  semi->black_space.ptr = malloc(sizeof(word) * semi->black_space.size);

  semi->grey_space.size = semi->white_space.size;
  // semi->grey_space.ptr = calloc(sizeof(word), semi->grey_space.size);
  semi->grey_space.ptr = malloc(sizeof(word) * semi->grey_space.size);
  semi->grey_space.free = semi->grey_space.ptr;
  semi->grey_space.scavenged = semi->grey_space.ptr;

  semi->black_space.free = semi->black_space.ptr;

  assert(area_used(semi->grey_space)==0);
  assert(area_used(semi->black_space)==0);

  semi->has_roots = false;
  s->gen1_collections++;

  stats_timer_begin(s, MiscTimer);
  free(old1);
  free(old2);
  touchSpace(semi->black_space.ptr, semi->black_space.size);
  touchSpace(semi->grey_space.ptr, semi->grey_space.size);
  stats_timer_end(s);
}

// void semi_scavenge_one(SemiSpace *semi) {
//   Header header;
//   header = readHeader(semi->grey_space.scavenged);
//   assert(header.data.grey == true);
//   header.data.grey = false;
//   header.data.black = semi->black_bit;
//   *semi->grey_space.scavenged = header.raw;
//
//   const uint8_t prims = header.data.prims;
//   const uint8_t ptrs = header.data.ptrs;
//   if(work < 1+prims+ptrs) work = 0;
//   else work -= 1+prims+ptrs;
//   semi->grey_space.scavenged += 1 + prims;
//   for(int i=0;i<ptrs;i++) {
//     semi_evacuate(semi, (hp*) semi->grey_space.scavenged);
//     semi->grey_space.scavenged++;
//   }
// }

/*   | white space |
 *   | grey space |
 *   | black space     |
 */
void semi_scavenge_concurrent(SemiSpace *semi, Stats *s) {
  const word scavengedWords = semi->grey_space.scavenged - semi->grey_space.ptr;

  // return;
  if(scavengedWords * semi->factor < area_used(semi->black_space)) {
    word work = area_used(semi->black_space) - scavengedWords * semi->factor;
    word* grey_scavenged = semi->grey_space.scavenged;
    // printf("Concurrent Scavenge: %lu %lu %lu\n", scavengedWords,area_used(semi->black_space),area_used(semi->grey_space));
    stats_timer_begin(s, Gen1Timer);
    while( work && grey_scavenged<semi->grey_space.free) {
      Header header = readHeader(grey_scavenged);
      assert(header.data.grey == true);
      header.data.grey = false;
      header.data.black = semi->black_bit;
      *grey_scavenged = header.raw;

      const uint8_t prims = header.data.prims;
      const uint8_t ptrs = header.data.ptrs;
      if(work < 1+prims+ptrs) work = 0;
      else work -= 1+prims+ptrs;
      grey_scavenged += 1 + prims;
      for(int i=0;i<ptrs;i++) {
        semi_evacuate(semi, (hp*) grey_scavenged);
        grey_scavenged++;
      }
    }
    semi->grey_space.scavenged = grey_scavenged;
    if(work) {
      semi_swap_colors(semi, s);
    }
    stats_timer_end(s);
  }
}

void semi_scavenge(SemiSpace *semi, Stats *s) {
  Header header;
  stats_timer_begin(s, Gen1Timer);
  // printf("Semi begin: work done: %lu\n", semi->grey_space.scavenged-semi->grey_space.ptr);
  hp scav_before = semi->grey_space.scavenged;
  while(semi->grey_space.scavenged<semi->grey_space.free) {
    header = readHeader(semi->grey_space.scavenged);
    assert(header.data.grey == true);
    header.data.grey = false;
    header.data.black = semi->black_bit;
    *semi->grey_space.scavenged = header.raw;

    const uint8_t prims = header.data.prims;
    const uint8_t ptrs = header.data.ptrs;
    semi->grey_space.scavenged += 1 + prims;
    for(int i=0;i<ptrs;i++) {
      semi_evacuate(semi, (hp*) semi->grey_space.scavenged);
      semi->grey_space.scavenged++;
    }
  }
  // printf("Semi begin: final work: %lu\n", semi->grey_space.free-scav_before);
  semi_swap_colors(semi,s);
  stats_timer_end(s);
}

word semi_size(SemiSpace *semi) {
  return semi->white_space.size + area_used(semi->grey_space) +
         area_used(semi->black_space);
}
