#include <sys/mman.h>
#include <stdio.h>
#include <stdint.h>
#include <errno.h>
#include <string.h>
#include <time.h>

// 16 mibi
#define SIZE (1<<24)
#define ITERATIONS 500
#define TOTAL_MEMORY ((SIZE/1024/1024)*ITERATIONS)

int main() {
  uint64_t *ptr;
  clock_t timing_start, timing_end;
  double timing_t;

  printf("Reusing memory vs. mapping new pages.\n");
  printf("Memory size: %d MiBi\n", SIZE/1024/1024);
  printf("Iterations:  %d\n", ITERATIONS);

  timing_start = clock();
  ptr = mmap(NULL, SIZE, PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0);
  for(uint64_t j=0;j<ITERATIONS;j++) {
    for(uint64_t i=0; i<SIZE/sizeof(uint64_t); i+=2) {
      ptr[i] = j+i;
      ptr[i+1] = j+i+1;
    }
  }
  munmap(ptr, SIZE);
  timing_end = clock();
  timing_t = ((double)(timing_end-timing_start))/CLOCKS_PER_SEC;
  printf("Reusing pages: %.2fs, %.2f MiBi/s\n", timing_t, (double)TOTAL_MEMORY/timing_t);


  timing_start = clock();
  for(uint64_t j=0;j<ITERATIONS;j++) {
    ptr = mmap(NULL, SIZE, PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0);
    for(uint64_t i=0; i<SIZE/sizeof(uint64_t); i+=2) {
      ptr[i] = j+i;
      ptr[i+1] = j+i+1;
    }
    munmap(ptr, SIZE);
  }
  timing_end = clock();
  timing_t = ((double)(timing_end-timing_start))/CLOCKS_PER_SEC;
  printf("Mapping new pages: %.2fs, %.2f MiBi/s\n", timing_t, (double)TOTAL_MEMORY/timing_t);
  return 0;
}
