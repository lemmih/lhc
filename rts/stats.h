#ifndef __LHC_STATS__
#define __LHC_STATS__

#include "api.h"

extern int _lhc_enable_gc_stats;

void _lhc_stats_init(void);
void _lhc_stats_collect(void);
void _lhc_stats_copy(int size);
void _lhc_stats_allocate(int size);
void _lhc_stats_heap(int heap);
void _lhc_stats_live(int live);
void _lhc_stats_scavenged(word *object);

#endif
