#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

/*
  0: 21.4%   1 kb 16.65
  1: 12.5%   2 kb 19.22
  2:  6.9%   4 kb 20.94
  3:  3.8%   8 kb 21.78
  4:  2.0%  16 kb 22.35
  5:  2.1%  32 kb 22.34
  6: 11.4%  64 kb 20.18
  7: 11.4% 128 kb 20.24
  8: 13.1% 256 kb 19.86
  9: 17.8% 512 kb 18.70
 10: 18.6%   1 mb 18.59
 11: 21.3%   2 mb 17.55
 12: 45.8%   4 mb 12.33
 13: 58.9%   8 mb  9.32
 14: 61.0%  16 mb  8.83
 15: 61.7%  32 mb  8.65
 16: 61.9%  64 mb  8.58
 17: 62.8% 128 mb  8.67
 */

#define WORD_SIZE   8
#define WORDS_PER_KILO (1024/WORD_SIZE)

#ifndef SHIFT
#define SHIFT       12
#endif

#define SIZE        (128<<SHIFT)
// #define ITERATIONS  ((WORDS_PER_KILO*2621440)>>SHIFT)
#define ITERATIONS  (HEAP_SIZE/SIZE*16)
#define OBJ_SIZE    4
// #define HEAP_SIZE   ((uint64_t)((uint64_t)SIZE*(uint64_t)ITERATIONS/NTH*OBJ_SIZE))
#define HEAP_SIZE   (uint64_t)(2*1024*1024*WORDS_PER_KILO)

int main() {
  uint64_t i,j,n;
  uint64_t *heap;
#if SIZE < 1024*1024
  uint64_t nursery[SIZE];
#else
  uint64_t *nursery;
  nursery = (uint64_t*) malloc(SIZE*WORD_SIZE);
  printf("Using malloc instead of stack.\n");
#endif
  // heap = (uint64_t*) malloc(HEAP_SIZE*WORD_SIZE);
  // printf("Heap:       %lu MiBi\n", HEAP_SIZE*WORD_SIZE/1024/1024);
  printf("Nursery:    %d KiBi\n", SIZE*WORD_SIZE/1024);
  printf("Iterations: %lu\n", ITERATIONS);
  printf("Memory being written: %lu MiBi\n", (SIZE*ITERATIONS/1024/1024)*WORD_SIZE);
  for(j=0;j<ITERATIONS;j++) {
    for(i=0;i<SIZE;i++) {
      nursery[i]=i;
    }
  }
  // for(j=0;j<(uint64_t)ITERATIONS*SIZE;j++) {
  //   nursery[j%SIZE]=j%SIZE;
  // }
  return (int)nursery[0]; // + *heap;
}
