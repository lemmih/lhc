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
  s->start_time = 0;
}

void stats_pprint(Stats *s) {
  word allocated        = s->allocated/1024/1024*8;
  word nursery_copied   = (s->nursery_copied/1024/1024*8);
  word nursery_survival = (nursery_copied*100) / allocated;

  printf("Nursery allocations %lu mb\n", allocated);
  printf("Gen1 allocations    %lu mb\n", nursery_copied);
  printf("Nursery survival:   %lu%%\n", nursery_survival);
  printf("Nursery GC rate:    %.2f mb/s\n", ((double)nursery_copied) / ((double)s->nursery_time / 1e9));
  printf("Nursery collections: %lu\n", s->nursery_n_collections);
  s->nursery_time = MAX(s->nursery_time, 1);
  s->gen1_collections = MAX(s->gen1_collections, 1);
  printf("Nursery time: %s", pp_time(s->nursery_time));
  printf(" (max %s", pp_time(s->nursery_time_max));
  printf(", avg %s)\n",
    pp_time(s->nursery_time/s->nursery_n_collections));
  printf("Gen1 time: %s", pp_time(s->gen1_time));
  printf(" (%s)\n", pp_time(s->gen1_time/s->gen1_collections));
}
