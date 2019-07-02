#include "marksweep.h"
#include "header.h"
#include "utils.h"
#include "stats.h"
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

void ms_init(MarkSweep *ms) {
  ms->black_bit = 0;
  ms->has_roots = false;
  ms->marked = 0;
  ms->allocated = 0;
  ms->factor = 2;
  ms->mark_stack = NULL;
  ms->blocks = NULL;
  ms->used = NULL;
}

bool ms_stack_is_empty(MarkSweep *ms) {
  while(ms->mark_stack != NULL) {
    if(ms->mark_stack->free > 0) return false;
    MarkStack *old = ms->mark_stack;
    ms->mark_stack = ms->mark_stack->prev;
    free(old);
  }
  return true;
}

void ms_stack_push(MarkSweep *ms, hp* objAddr) {
  if(ms->mark_stack == NULL) {
    MarkStack *new = malloc(sizeof(MarkStack));
    ms->mark_stack = new;
  } else if(ms->mark_stack->free == sizeof(ms->mark_stack->stack)/sizeof(*ms->mark_stack->stack)) {
    MarkStack *new = malloc(sizeof(MarkStack));
    new->prev = ms->mark_stack;
    ms->mark_stack = new;
  }
  ms->mark_stack->stack[ms->mark_stack->free++] = objAddr;
}

hp* ms_stack_pop(MarkSweep *ms) {
  while(ms->mark_stack != NULL) {
    if(ms->mark_stack->free > 0) {
      ms->mark_stack->free--;
      return ms->mark_stack->stack[ms->mark_stack->free];
    }
    MarkStack *old = ms->mark_stack;
    ms->mark_stack = ms->mark_stack->prev;
    free(old);
  }
  return NULL;
}

void ms_mark_object(unsigned int selector, hp obj) {
  Block *block = GET_BLOCK_PTR(obj);
  int bit = (obj - (hp)block) / MS_CHUNK_SIZE;
  for(int i=0; i<MS_BITMAP_SIZE; i++) {
    if(bit < sizeof(word)*8) {
      block->bitmap[selector][i] |= 1<<bit;
      return;
    }
    bit -= sizeof(word)*8;
  }
}

void ms_print_stats(MarkSweep *ms) {
  word blocks = 0;
  word used = 0;
  word allocated = 0;
  word marked = 0;

  Block *b = ms->blocks;
  while(b) {
    blocks++;
    used += b->used;
    marked += ms_block_popcount(b)*MS_CHUNK_SIZE;
    b = b->next;
  }

  b = ms->used;
  while(b) {
    blocks++;
    allocated += MS_BLOCK_ENTRIES;
    b = b->next;
  }

  printf(
    "Blocks used:     %4" PRIu64 "\n"
    "Resident mem:    %s\n"
    "Traced mem:      %s\n"
    "Heap mem:        %s (new: %s)\n"
    "Marked mem:      %s\n"
    "Traced/Marked:   %4" PRIu64 "%%\n"
    "Traced/Resident: %4" PRIu64 "%%\n"
    "Heap/Resident:   %4" PRIu64 "%%\n"
    , blocks
    , pp_bytes(blocks*MS_BLOCK_SIZE/sizeof(word))
    , pp_bytes(used)
    , pp_bytes(used+allocated), pp_bytes(allocated)
    , pp_bytes(marked)
    , marked?used*100/(marked):0
    , blocks?used*sizeof(word)*100/(blocks*MS_BLOCK_SIZE):0
    , blocks?(used+allocated)*sizeof(word)*100/(blocks*MS_BLOCK_SIZE):0
  );
}

void ms_swap_colors(MarkSweep *ms) {
  // flip color bit.
  // clear mark bits.
  // clear used counts.
  ms->black_bit = !ms->black_bit;

  ms_print_stats(ms);

  Block *b = ms->blocks;
  while(b) {
    for(int i=0; i<MS_BITMAP_SIZE; i++)
      b->bitmap[!ms->black_bit][i] = 0;
    b->used = 0;
    b = b->next;
  }

  b = ms->used;
  while(b) {
    for(int i=0; i<MS_BITMAP_SIZE; i++) {
      b->bitmap[!ms->black_bit][i] = 0;
      b->bitmap[ms->black_bit][i] = ~0;
    }
    b->used = 0;
    b = b->next;
  }

  while((b = ms->used)) {
    ms->used = b->next;
    if(ms->used)
      ms->used->prev = NULL;
    b->next = ms->blocks;
    ms->blocks = b;
    if(b->next)
      b->next->prev = b;
  }
}

static void ms_mark_one(MarkSweep *ms) {
  // if(ms_stack_is_empty(ms)) break;
  hp *objAddr = ms_stack_pop(ms);
  if(objAddr == NULL) return;
  printf("Mark one: Got object addr\n");
  hp obj = *objAddr;

  printf("Mark one: Found object %p\n", obj);

  // FIXME: Check that obj is in gen2 without touching it.

  Header header = readHeader(obj);
  while( header.data.isForwardPtr ) {
    obj = (hp) ((word)header.forwardPtr & (~1));
    *objAddr = obj;
    // FIXME: Check that obj is in gen2 without touching it.
    header = readHeader(obj);
  }
  printf("Mark one: Followed object\n");
  assert( header.data.isForwardPtr == 0);

  const uint8_t prims = getObjectPrims(header);
  const uint8_t ptrs = getObjectPtrs(header);
  const word obj_size = 1+prims+ptrs;

  printf("Mark one: Unpacked object: %" PRIu64 " %d %d %d\n", obj_size, prims, ptrs, header.data.gen);
  switch( header.data.gen ) {
    case 0:
      return;
    case 1:
      return;
    case 2:
      if(header.data.black == ms->black_bit) break;
      header.data.black = ms->black_bit;
      *obj = header.raw;
      ms_mark_object(!ms->black_bit, obj);
      GET_BLOCK_PTR(obj)->used += obj_size;
      ms->marked += obj_size;
      for(int i=0; i<ptrs; i++)
        ms_stack_push(ms, (hp*) obj+1+prims+i);
      return;
    default:
      __builtin_unreachable();
      // abort();
  }
}

void ms_mark_concurrent(MarkSweep *ms) {
  while(ms->marked * ms->factor < ms->allocated && ms->mark_stack) {
    ms_mark_one(ms);
  }
}

void ms_mark_all(MarkSweep *ms) {
  // printf("Stack %p\n", ms->mark_stack);
  while(ms->mark_stack) {
    ms_mark_one(ms);
  }
  ms_swap_colors(ms);
}


Block *ms_reserve_block(MarkSweep *ms) {
  Block *ret;
  if(ms->blocks != NULL) {
    ret = ms->blocks;
    ms->blocks = ret->next;
    ms->blocks->prev = NULL;
  } else {
    ret = mmap_aligned(MS_BLOCK_SIZE, MS_BLOCK_SIZE, PROT_READ|PROT_WRITE,
                       MAP_ANONYMOUS | MAP_PRIVATE);
  }
  ret->next = ms->used;
  if(ret->next)
    ret->next->prev = ret;
  ms->used = ret;
  assert(ret == GET_BLOCK_PTR(ret));
  return ret;
}
int ms_block_popcount(Block *block) {
  int count = 0;
  for(int i=0; i<MS_BITMAP_SIZE;i++)
    count += __builtin_popcountll(block->bitmap[0][i]);
  return count;
}
