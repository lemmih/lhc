#include "stats.h"
#include "common.h"
#include <stdint.h>
#include <time.h>

void stats_init(Stats *s) {
  s->allocated = 0;
  s->nursery_n_collections = 0;
  s->nursery_time = 0;
  s->nursery_time_max = 0;
  s->nursery_copied = 0;
  s->gen1_collections = 0;
  s->gen1_time = 0;
  s->gen1_copied = 0;
  s->nursery_start_time = 0;
  s->gen1_start_time = 0;

  s->mut_start_time = clock_gettime_nsec(CLOCK_ID);

  s->misc_start_time=0;
  s->misc_time=0;

  s->max_heap = 0;
  s->max_residency = 0;
}

void stats_pprint(Stats *s) {
  uint64_t mut_end_time = clock_gettime_nsec(CLOCK_ID);
  uint64_t mut_time = mut_end_time - s->mut_start_time - s->nursery_time - s->gen1_time - s->misc_time;
  word allocated        = s->allocated*8/1024/1024;
  word nursery_copied   = (s->nursery_copied*8/1024/1024);
  word gen1_copied   = (s->gen1_copied*8/1024/1024);
  word nursery_survival = (nursery_copied*100) / allocated;

  /*
                  Mutator     Nursery      Gen 1
   Time:          10ms        10ms         10ms
   Rate:          1823 MB/s   2010 MB/s    1500 MB/s
   Written:       2288 MB     2288 MB      768 MB
   Survival:                  100%
   Productivity:  33%
   Collections:               1520         50
   Average pause:             124 us
   Max pause:                 503 us
  */
  printf("Mut time:           %s\n", pp_time(mut_time));
  printf("Allocation rate:    %.2f mb/s\n", allocated/((double)mut_time/1e9));
  printf("Nursery time:       %s", pp_time(s->nursery_time));
  printf(", max %s", pp_time(s->nursery_time_max));
  printf(", avg %s\n",
    pp_time(s->nursery_time/s->nursery_n_collections));
  printf("Productivity:       %.1f%%\n", (double)(mut_time*100)/(mut_time+s->nursery_time));
  printf("Gen1 time:          %s\n", pp_time(s->gen1_time));
  printf("Gen1 collections:   %lu\n", s->gen1_collections);
  printf("Misc time:          %s\n", pp_time(s->misc_time));


  printf("Nursery allocations %lu mb\n", allocated);
  printf("Gen1 allocations    %lu mb\n", nursery_copied);
  printf("Gen1 copied         %lu mb\n", gen1_copied);
  printf("Nursery survival:   %lu%%\n", nursery_survival);
  printf("Nursery GC rate:    %.2f mb/s\n", ((double)nursery_copied) / ((double)s->nursery_time / 1e9));
  printf("Nursery collections: %lu\n", s->nursery_n_collections);
  s->nursery_time = MAX(s->nursery_time, 1);
  s->gen1_collections = MAX(s->gen1_collections, 1);
  printf("Max heap:           %lu MB\n", s->max_heap*8/1024/1024);
  printf("Max residency:      %lu MB (%lu samples)\n", s->max_residency*8/1024/1024,s->gen1_collections);
}
