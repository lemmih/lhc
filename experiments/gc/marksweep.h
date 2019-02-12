#ifndef __MARKSWEEP_H__
#define __MARKSWEEP_H__

#include "common.h"
#include <stdbool.h>

#define MS_BLOCK_SIZE (32*1024)
#define MS_BITMAP_SIZE 64 // one cache line, 8 words, 512 bits
#define MS_CHUNK_SIZE (MS_BLOCK_SIZE/sizeof(word)/(MS_BITMAP_SIZE*sizeof(word)*8))
#define MS_BLOCK_ENTRIES ((MS_BLOCK_SIZE/sizeof(word))-MS_BITMAP_SIZE*2-3)
#define MS_STACK_ENTRIES ((MS_BLOCK_SIZE/sizeof(word))-2)

typedef struct _MarkStack {
  struct _MarkStack *prev;
  word free;
  hp* stack[MS_STACK_ENTRIES];
} MarkStack;

typedef struct _Block {
  word bitmap[2][MS_BITMAP_SIZE];
  struct _Block *prev, *next;
  word used;
  word data[MS_BLOCK_ENTRIES];
} Block;

typedef struct {
  unsigned int black_bit:1;
  bool has_roots;
  word marked;
  word allocated;
  double factor;
  MarkStack *mark_stack;
  Block *blocks;
  Block *used;
} MarkSweep;

#define BLOCK_MASK (~(MS_BLOCK_SIZE-1))
#define GET_BLOCK_PTR(obj) ((Block*)(((word)obj)&BLOCK_MASK))


void ms_init(MarkSweep*);
void ms_stack_push(MarkSweep*, hp*);
hp* ms_stack_pop(MarkSweep *);
void ms_mark_object(unsigned int, hp);
Block *ms_reserve_block(MarkSweep*);
int ms_block_popcount(Block*);

void ms_mark_all(MarkSweep*);

#endif
