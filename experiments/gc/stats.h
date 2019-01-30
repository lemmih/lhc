#ifndef __STATS_H__
#define __STATS_H__

#include <stdint.h>
#include <time.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <stdio.h>

typedef uint64_t word;

// enum Timer{MutTimer, Gen0Timer, Gen1Timer, MiscTimer, MaxTimer};

typedef struct {
  uint64_t allocated;
  uint64_t nursery_n_collections;
  uint64_t nursery_time;
  uint64_t nursery_time_max;
  uint64_t nursery_copied;

  uint64_t gen1_collections;
  uint64_t gen1_copied;
  uint64_t gen1_time;

  uint64_t nursery_start_time;
  uint64_t gen1_start_time;
  uint64_t mut_start_time;

  uint64_t misc_start_time;
  uint64_t misc_time;

  uint64_t max_heap;
  uint64_t max_residency;
} Stats;

void stats_init(Stats *s);
static void stats_nursery_begin();
static void stats_nursery_end(Stats *s);
void stats_pprint(Stats *s);

static uint64_t clock_gettime_nsec(clockid_t clk_id) {
  struct timespec t;
  clock_gettime(clk_id,&t);
  return (uint64_t)t.tv_sec*1e9 + t.tv_nsec;
}

static char *pp_time(uint64_t time) {
  static int i=0;
  static char buffer[10][128];
  i = (i+1) % 10;
  if(time < 1000000) {
    snprintf(buffer[i], 1024, "%.0fus", ((double)time)/1000);
  } else if(time < 1000000000) {
    snprintf(buffer[i], 1024, "%.2fms", ((double)time)/1000000);
  } else {
    snprintf(buffer[i], 1024, "%.2fs", ((double)time)/1000000000);
  }
  return buffer[i];
}


#define CLOCK_ID CLOCK_THREAD_CPUTIME_ID

static void stats_nursery_begin(Stats *s) {
  s->nursery_start_time = clock_gettime_nsec(CLOCK_ID);
  // s->start_time = clock_gettime_nsec(CLOCK_MONOTONIC_COARSE);
  // {
  //   struct rusage usage;
  //   getrusage(RUSAGE_SELF, &usage);
  //   printf("Page faults: %ld -> ", usage.ru_minflt);
  // }
}
static void stats_nursery_end(Stats *s) {
  uint64_t end_time = clock_gettime_nsec(CLOCK_ID);
  // uint64_t end_time = clock_gettime_nsec(CLOCK_MONOTONIC_COARSE);
  uint64_t time = end_time - s->nursery_start_time;
  s->nursery_time += time;
  if(time > s->nursery_time_max) {
    s->nursery_time_max = time;
    // printf("New max time: %s\n", pp_time(time));
  }

  // {
  //   struct rusage usage;
  //   getrusage(RUSAGE_SELF, &usage);
  //   printf("%ld %ld %ld %ld\n", usage.ru_minflt, time, usage.ru_nvcsw, usage.ru_nivcsw);
  // }

}

static void stats_gen1_begin(Stats *s) {
  s->gen1_start_time = clock_gettime_nsec(CLOCK_ID);
}
static void stats_gen1_end(Stats *s) {
  uint64_t end_time = clock_gettime_nsec(CLOCK_ID);
  s->gen1_time += end_time - s->gen1_start_time;
}

static void stats_misc_begin(Stats *s) {
  s->misc_start_time = clock_gettime_nsec(CLOCK_ID);
}
static void stats_misc_end(Stats *s) {
  uint64_t end_time = clock_gettime_nsec(CLOCK_ID);
  s->misc_time += end_time - s->misc_start_time;
}

#endif
