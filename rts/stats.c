#include <stdlib.h>
#include <stdio.h>

#include "stats.h"
#include "api.h"


/*
Objects allocated.
Bytes allocated.
Objects copied.
Bytes copied.
*/

int _lhc_stats_copied = 0;
int _lhc_stats_allocated = 0;
int _lhc_stats_collections = 0;

#define _LHC_HISTOGRAM_SIZE 64
int _lhc_distance_histogram[_LHC_HISTOGRAM_SIZE];

const int _lhc_buckets[] = {64, 4*1024, 64*1024, 512*1024, 2*1024*1024};
// 64bytes, 4kb, 64kb, 512kb, 2mb
int _lhc_distances[sizeof(_lhc_buckets)/sizeof(_lhc_buckets[0])+1];

int _lhc_cumulative_distance = 0;
int _lhc_cumulative_objects = 0;

int _lhc_enable_gc_stats = 0;


void _lhc_stats_finish(int status, void* data) {
  if(_lhc_enable_gc_stats) {
    printf("Exit. Printing stats. %d\n", _lhc_enable_gc_stats);
  }
}

void _lhc_stats_init(void) {
  on_exit(&_lhc_stats_finish, NULL);
}


void _lhc_stats_collect(void) {
  _lhc_stats_collections++;
}
void _lhc_stats_copy(int size) {
  _lhc_stats_copied += size;
}
void _lhc_stats_allocate(int size) {
  _lhc_stats_allocated += size;
}
void _lhc_stats_scavenged(word *object) {

}
