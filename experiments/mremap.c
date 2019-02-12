#define _GNU_SOURCE
#include <sys/mman.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <time.h>

// 16 mibi
#define SIZE (1<<24)
#define ITERATIONS 500
#define TOTAL_MEMORY ((uint64_t)1024*1024*1024*8) // 8gb

char* fmt(size_t n) {
  #define FMT_SIZE 1024
  #define CHECK(str) if(n < 1024) { snprintf(buffer, FMT_SIZE, "%3lu " str, n); return buffer; } else { n/=1024;}
  static char buffer[FMT_SIZE];
  CHECK("");
  CHECK("kb");
  CHECK("mb");
  CHECK("gb");
  CHECK("tb");
  return NULL;
}

void test(size_t page_shift, bool reuse) {
  uint8_t *ptr;
  size_t size = sysconf(_SC_PAGESIZE)<<page_shift;

  printf("Chunk size: %s. ", fmt(size));

  ptr = mmap(NULL, TOTAL_MEMORY, PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0);
  if(ptr == MAP_FAILED) {
    printf("Error: %s\n", strerror(errno));
    abort();
  }
  for(uint64_t j=0;j<TOTAL_MEMORY;j+=size) {
    // printf("Writing chunk: %lu %lu\n", j, TOTAL_MEMORY);
    memset(ptr+j, (int)j, size);
    if(reuse) {
      if(j+size<TOTAL_MEMORY) {
        if(mremap(ptr+j, size, size, MREMAP_MAYMOVE | MREMAP_FIXED, ptr+j+size) == MAP_FAILED) {
          printf("Error: %s\n", strerror(errno));
          abort();
        }
      } else {
        munmap(ptr+j, size);
      }
    } else {
      munmap(ptr+j, size);
    }
  }
}

int main() {
  clock_t timing_start, timing_end;
  double timing_t;

  printf("This programs tests whether it is beneficial to reuse pages.\n");

  printf("This first test will reuse pages (using mremap) to avoid page faults and page zeroing.\n");

  for(int i=0;i<22;i++) {
    timing_start = clock();
    test(i,1);
    timing_end = clock();
    timing_t = ((double)(timing_end-timing_start))/CLOCKS_PER_SEC;
    printf("Reusing pages: %.2fs, %5.2f GiBi/s\n", timing_t, (double)TOTAL_MEMORY/timing_t/1024/1024/1024);
  }
  printf("\nThis second test writes the same amount of memory but doesn't reuse dirty pages.\n");
  for(int i=0;i<22;i++) {
    timing_start = clock();
    test(i,0);
    timing_end = clock();
    timing_t = ((double)(timing_end-timing_start))/CLOCKS_PER_SEC;
    printf("Zeroing pages: %.2fs, %2.2f GiBi/s\n", timing_t, (double)TOTAL_MEMORY/timing_t/1024/1024/1024);
  }
  return 0;
}
