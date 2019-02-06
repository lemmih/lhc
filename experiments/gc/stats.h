#ifndef __STATS_H__
#define __STATS_H__

#include <stdint.h>
#include <time.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

typedef uint64_t word;

#define TIMER_STACK 8
enum Timer{MutTimer, Gen0Timer, Gen1Timer, MiscTimer, MaxTimer};

typedef struct {
  uint64_t allocated;
  uint64_t nursery_n_collections;
  uint64_t nursery_time_max;
  uint64_t nursery_copied;

  uint64_t gen1_collections;
  uint64_t gen1_copied;

  uint64_t max_heap;
  uint64_t max_residency;

  uint64_t start_time;
  enum Timer active_timer[TIMER_STACK];
  uint64_t n_timers;
  uint64_t timers[MaxTimer];
} Stats;

void stats_init(Stats *s);
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
    snprintf(buffer[i], 128, "%4.0f us", ((double)time)/1000);
  } else if(time < 10000000) {
    snprintf(buffer[i], 128, "%4.2f ms", ((double)time)/1000000);
  } else if(time < 100000000) {
    snprintf(buffer[i], 128, "%4.1f ms", ((double)time)/1000000);
  } else if(time < 1000000000) {
    snprintf(buffer[i], 128, "%4.0f ms", ((double)time)/1000000);
  } else {
    snprintf(buffer[i], 128, "%4.2f s", ((double)time)/1000000000);
  }
  return buffer[i];
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

static char *pp_bytes(uint64_t words) {
  static int i=0;
  static char buffer[10][128];
  uint64_t bytes = words * 8;
  i = (i+1) % 10;
  if(bytes < 1024) {
    snprintf(buffer[i], 128, "%4lu B", bytes);
  } if(bytes < 1024*1024) {
    snprintf(buffer[i], 128, "%4lu KB", bytes/1024);
  } if(bytes < (uint64_t)1024*1024*1024*10) {
    snprintf(buffer[i], 128, "%4lu MB", bytes/1024/1024);
  } else {
    snprintf(buffer[i], 128, "%4.1f GB", ((double)bytes)/1024/1024/1024);
  }
  return buffer[i];
}


#define CLOCK_ID CLOCK_PROCESS_CPUTIME_ID

static void stats_timer_begin(Stats *s, enum Timer t) {
  uint64_t now = clock_gettime_nsec(CLOCK_ID);
  assert(s->n_timers < TIMER_STACK);
  s->active_timer[s->n_timers] = t;
  if(s->n_timers > 0) {
    s->timers[s->active_timer[s->n_timers-1]] += now - s->start_time;
  }

  s->n_timers++;
  s->start_time = now;
}
static void stats_timer_end(Stats *s) {
  uint64_t now = clock_gettime_nsec(CLOCK_ID);
  uint64_t t = now - s->start_time;
  assert(s->n_timers > 0);
  if(s->active_timer[s->n_timers-1] == Gen0Timer) {
    if(t > s->nursery_time_max) {
      s->nursery_time_max = t;
    }
  }
  s->timers[s->active_timer[s->n_timers-1]] += t;
  s->start_time = now;
  s->n_timers--;
}

#endif
