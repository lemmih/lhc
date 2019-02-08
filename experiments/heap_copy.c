#include <sys/mman.h>
#include <stdint.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <stdbool.h>
#include <assert.h>

void touchSpace(uint64_t *ptr, uint64_t size) {
  for(int i=0; i < size; i+=512) {
    ptr[i] = i;
  }
}

static char *pp_speed(uint64_t words, uint64_t time) {
  static int i=0;
  static char buffer[10][128];
  uint64_t rate = words*8/1024 / (time/1e9);
  i = (i+1) % 10;
  if(rate < 1024) {
    snprintf(buffer[i], 128, "%4lu KB/s", rate);
  } if(rate < 1024*1024*10) { // Less than 10000 MB/s
    snprintf(buffer[i], 128, "%4lu MB/s", rate/1024);
  } else {
    snprintf(buffer[i], 128, "%4.1f GB/s", (double)rate/1024/1024);
  }
  return buffer[i];
}

static uint64_t clock_gettime_nsec(clockid_t clk_id) {
  struct timespec t;
  clock_gettime(clk_id,&t);
  return (uint64_t)t.tv_sec*1e9 + t.tv_nsec;
}

// From-area
// To-area
// size in words.
void heap_copy(uint64_t *from_area, uint64_t from_size, uint64_t *to_area, uint64_t to_size, bool temporal, int object_size) {
  uint64_t *to_limit = to_area + to_size - object_size;
  if(temporal) {
    while(to_area < to_limit) {
      long unsigned int n = (long unsigned int)random();
      n %= (from_size-object_size);
      for(int i=0;i<object_size;i++) {
        *to_area++ = from_area[n+i];
      }
      from_area[n] = (uint64_t)to_area;
    }
  } else {
    while(to_area < to_limit) {
      long unsigned int n = (long unsigned int)random();
      n %= (from_size-object_size);
      for(int i=0;i<object_size;i++) {
        // *to_area++ = from_area[i];
        uint64_t val = __builtin_nontemporal_load(from_area+i);
        // uint64_t val = from_area[n+i];
        __builtin_nontemporal_store(val,to_area);
        // *to_area = val;
        to_area++;
      }
      __builtin_nontemporal_store((uint64_t)to_area,from_area+n);
    }
  }
}



int main() {
  uint64_t *from_area, *to_area;
  uint64_t from_size, to_size;

  from_size = (uint64_t)1024*1024*512 / sizeof(uint64_t);
  to_size   = (uint64_t)1024*1024*1024*1 / sizeof(uint64_t);

  from_area = mmap(NULL, from_size*sizeof(uint64_t), PROT_READ|PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  to_area = mmap(NULL, to_size*sizeof(uint64_t), PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  assert(from_area != NULL);
  assert(to_area != NULL);
  uint64_t start = clock_gettime_nsec(CLOCK_PROCESS_CPUTIME_ID);
  touchSpace(from_area, from_size);
  touchSpace(to_area, to_size);
  uint64_t total = clock_gettime_nsec(CLOCK_PROCESS_CPUTIME_ID) - start;
  printf("Zeroed pages: %s\n", pp_speed(to_size+from_size, total));

  printf("%6s%15s%15s\n","Size","Non-Temporal","Temporal");
  for(int object_size=1; object_size<=16; object_size++) {
    start = clock_gettime_nsec(CLOCK_PROCESS_CPUTIME_ID);
    heap_copy(from_area, from_size, to_area, to_size, false, object_size);
    total = clock_gettime_nsec(CLOCK_PROCESS_CPUTIME_ID) - start;
    printf("%6d%15s", object_size, pp_speed(to_size, total));

    start = clock_gettime_nsec(CLOCK_PROCESS_CPUTIME_ID);
    heap_copy(from_area, from_size, to_area, to_size, true, object_size);
    total = clock_gettime_nsec(CLOCK_PROCESS_CPUTIME_ID) - start;
    printf("%15s\n", pp_speed(to_size, total));
  }
  return 0;
}
