#include "stats.h"
#include "common.h"
#include <stdint.h>
#include <string.h>
#include <time.h>

void stats_init(Stats *s) {
  s->allocated = 0;
  s->nursery_n_collections = 0;
  s->nursery_time_max = 0;
  s->nursery_copied = 0;
  s->gen1_collections = 0;
  s->gen1_copied = 0;

  s->max_heap = 0;
  s->max_residency = 0;
  s->start_time = 0;
  memset(s->timers,  0, sizeof(s->timers));
  s->n_timers=0;

  s->latency_50 = gsl_rstat_quantile_alloc(0.50);
  s->latency_90 = gsl_rstat_quantile_alloc(0.90);
  s->latency_99 = gsl_rstat_quantile_alloc(0.99);
}

void stats_pprint(Stats *s) {
  while(s->n_timers) stats_timer_end(s);
  uint64_t mut_time = s->timers[MutTimer];
  uint64_t nursery_time = s->timers[Gen0Timer];
  word allocated        = s->allocated*8/1024/1024;
  word nursery_copied   = (s->nursery_copied*8/1024/1024);
  word gen1_copied   = (s->gen1_copied*8/1024/1024);
  word nursery_survival = (nursery_copied*100) / MAX(1,allocated);
  word gen1_survival = nursery_copied==0?0:(gen1_copied*100) / nursery_copied;

  /*
                  Mutator     Nursery      Gen 1
   Time:          10ms        10ms         10ms
   Rate:          1823 MB/s   2010 MB/s    1500 MB/s
   Written:       2288 MB     2288 MB      768 MB
   Survival:                  100%         30%
   Productivity:  33%
   Collections:                            50
   Average pause:             124 us
   Max pause:                 503 us
   Max heap:                  1 MB         123 MB
   Max residency:             59 KB        95 MB
  */

  printf(
    "              Mutator        Nursery        Gen 1\n"
    "Time:         %-15s%-15s%s\n"
    "Rate:         %-15s%-15s%s\n"
    "Written:      %-15s%-15s%s\n"
    "Survival:                     %3lu%%           %3lu%%\n"
    "Productivity: %3.0f%%           %-15s%s\n"
    "\n"
    "Pauses:        50%%     90%%     99%%     100%%\n"
    "              %-8s%-8s%-8s%-8s\n"
    "Collections:                                %4lu\n"
    "\n"
    , pp_time(s->timers[MutTimer])
    , pp_time(s->timers[Gen0Timer])
    , pp_time(s->timers[Gen1Timer])
    , pp_speed(s->allocated,s->timers[MutTimer])
    , pp_speed(s->nursery_copied,s->timers[Gen0Timer])
    , pp_speed(s->gen1_copied,s->timers[Gen1Timer])
    , pp_bytes(s->allocated)
    , pp_bytes(s->nursery_copied)
    , pp_bytes(s->gen1_copied)
    , nursery_survival
    , nursery_copied==0?0:(gen1_copied*100) / nursery_copied
    , (double)(mut_time*100)/(MAX(mut_time+nursery_time,1))
    , pp_speed(s->nursery_copied, s->timers[MutTimer] + s->timers[Gen0Timer])
    , pp_speed(gen1_survival?s->gen1_copied/gen1_survival*100:0,s->timers[Gen1Timer])
    , pp_time((uint64_t)gsl_rstat_quantile_get(s->latency_50))
    , pp_time((uint64_t)gsl_rstat_quantile_get(s->latency_90))
    , pp_time((uint64_t)gsl_rstat_quantile_get(s->latency_99))
    , pp_time(s->nursery_time_max)
    , s->gen1_collections
  );

  printf("Total time:         %s\n", pp_time(s->timers[MutTimer]+s->timers[Gen0Timer]+s->timers[Gen1Timer]));
  printf("Misc time:          %s\n", pp_time(s->timers[MiscTimer]));
  printf("Max heap:           %lu MB\n", s->max_heap*8/1024/1024);
  printf("Max residency:      %lu MB (%lu sample(s))\n", s->max_residency*8/1024/1024,s->gen1_collections);
}
