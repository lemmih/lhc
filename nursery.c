#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

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

#define STEPS 17
#define WORD_SIZE ((uint64_t)sizeof(uint64_t))
// write size in kb. set to 32gb.
#define WRITE_SIZE (1024*1024*32)

int main() {
  uint64_t i,j,n;
  uint64_t *nursery;
  printf("Allocating nursery, %d mb:\t", (1024<<(STEPS-1))/1024/1024);
  fflush(stdout);
  nursery = (uint64_t*) malloc(1024<<(STEPS-1));
  printf("Done.\n");
  // heap = (uint64_t*) malloc(HEAP_SIZE*WORD_SIZE);
  // printf("Heap:       %lu MiBi\n", HEAP_SIZE*WORD_SIZE/1024/1024);
  // printf("Nursery:    %d KiBi\n", SIZE*WORD_SIZE/1024);
  // printf("Iterations: %lu\n", ITERATIONS);
  // printf("Memory being written: %lu MiBi\n", (SIZE*ITERATIONS/1024/1024)*WORD_SIZE);
  printf("Warm up:\t\t\t");
  fflush(stdout);
  // Warm up
  for(n=0;n<3;n++) {
    uint64_t size = 128<<n;
    uint64_t iterations = WRITE_SIZE>>n;
    for(j=0;j<iterations;j++) {
      for(i=0;i<size;i++) {
        nursery[i]=i;
      }
    }
  }
  printf("Done.\n");

  for(n=0;n<STEPS;n++) {
    uint64_t size = 128<<n;
    uint64_t iterations = WRITE_SIZE>>n;
    clock_t start, end;
    printf("Testing nusery size: %5lu kb:\t", (size*WORD_SIZE)/1024);
    fflush(stdout);
    start = clock();
    for(j=0;j<iterations;j++) {
      for(i=0;i<size;i++) {
        nursery[i]=i;
      }
    }
    end = clock();
    printf("%.2f gb/s\n", WRITE_SIZE/((end-start)/(double)CLOCKS_PER_SEC)/1024/1024);
  }
  // for(j=0;j<(uint64_t)ITERATIONS*SIZE;j++) {
  //   nursery[j%SIZE]=j%SIZE;
  // }
  return (int)nursery[0]; // + *heap;
}
