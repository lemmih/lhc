#ifndef __LHC_STATS__
#define __LHC_STATS__

#include "api.h"

extern uint64_t _lhc_stats_allocated;

void _lhc_stats_init(void);
void _lhc_stats_start_gc();
void _lhc_stats_end_gc();
void _lhc_stats_collect(void);
void _lhc_stats_copy(uint64_t size);
void _lhc_stats_allocate(uint64_t size);
void _lhc_stats_heap(uint64_t heap);
void _lhc_stats_live(uint64_t live);
void _lhc_stats_scavenged(word *object);
void _lhc_stats_tail_evacuation();
void _lhc_stats_evacuation();

#endif
