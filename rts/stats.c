#include <stdlib.h>
#include <stdio.h>

#include "stats.h"
#include "api.h"

char* format(int, uint64_t);

/*
Objects allocated.
Bytes allocated.
Objects copied.
Bytes copied.
*/

static uint64_t _lhc_stats_copied = 0;
static uint64_t _lhc_stats_allocated = 0;
static int _lhc_stats_collections = 0;
static int _lhc_stats_max_heap = 0;
static int _lhc_stats_max_copied = 0;

#define _LHC_HISTOGRAM_SIZE 64
static int _lhc_distance_histogram[_LHC_HISTOGRAM_SIZE];

const int _lhc_buckets[] = {64, 4*1024, 64*1024, 512*1024, 2*1024*1024};
#define _LHC_N_BUCKETS (sizeof(_lhc_buckets)/sizeof(_lhc_buckets[0]))
// 64bytes, 4kb, 64kb, 512kb, 2mb
static int _lhc_distances[_LHC_N_BUCKETS+1];

static int _lhc_cumulative_distance = 0;
static int _lhc_cumulative_objects = 0;

int _lhc_enable_gc_stats = 0;


void _lhc_stats_finish(int status, void* data) {
  if(_lhc_enable_gc_stats) {
    fprintf(stderr, "Collections: %d\n", _lhc_stats_collections);
    fprintf(stderr, "Copied:      %s\n", format(1,_lhc_stats_copied));
    fprintf(stderr, "Allocated:   %s\n", format(1,_lhc_stats_allocated));
    fprintf(stderr, "Max heap:    %s\n", format(1,_lhc_stats_max_heap));
    fprintf(stderr, "Max copied:  %s\n", format(1,_lhc_stats_max_copied));
    int i;
    for(i=0; i<_LHC_N_BUCKETS;i++) {
      fprintf(stderr, "Bucket:      <%-6s %d\n", format(0,_lhc_buckets[i]), _lhc_distances[i]);
    }
    fprintf(stderr, "Bucket:      >%-6s %d\n", format(0,_lhc_buckets[i-1]), _lhc_distances[i]);
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
void _lhc_stats_heap(int heap) {
  if(_lhc_stats_max_heap < heap)
    _lhc_stats_max_heap = heap;
}
void _lhc_stats_live(int live) {
  if(_lhc_stats_max_copied < live)
    _lhc_stats_max_copied = live;
}
void _lhc_stats_scavenged(word *scavenged) {
  InfoTable *table;
  table = &_lhc_info_tables[_lhc_getTag(*scavenged)];
  for(int i=0;i<table->nHeapPointers;i++) {
    word *new_addr = ((word**)scavenged)[1+table->nPrimitives+i];
    int distance = (new_addr-scavenged)*sizeof(word);
    int j;
    for(j=0; j<_LHC_N_BUCKETS; j++)
      if( distance < _lhc_buckets[j] )
        break;
    _lhc_distances[j]++;
  }
}

//////////////////////////////////////////////////////
// Helpers



// caution: Uses a static buffer.
char* format(int precision, uint64_t number) {
  static char buffer[1024];
  double n = (double)number;
  if(number < 1024) {
    sprintf(buffer, "%.*f b", precision, n);
  } else if(number < 1024*1024) {
    sprintf(buffer, "%.*f kb", precision, n/1024);
  } else if(number < 1024*1024*1024) {
    sprintf(buffer, "%.*f mb", precision, n/1024/1024);
  } else {
    sprintf(buffer, "%.*f gb", precision, n/1024/1024/1024);
  }
  return buffer;
}
