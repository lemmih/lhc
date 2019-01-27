#define _GNU_SOURCE
#include "common.h"
#include "nursery.h"
#include "semispace.h"
#include "objects.h"
#include "stats.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <unistd.h>
#include <errno.h>
#include <sched.h>

static void warmup(Nursery *ns, SemiSpace *semi) {
  Stats s;
  hp obj, new=NULL;
  obj = allocate(ns, semi, Zero, (MkZero){});
  assert(obj!=NULL);

  for(;;) {
    new = allocate(ns, semi, Branch, (MkBranch){obj, obj});
    if(new==NULL) {
      // printf("Nursery full\n");
      nursery_evacuate(ns, semi, &obj);
      nursery_reset(ns, &s);
      semi_scavenge(semi, &s); // Turn black objects white.
      semi_scavenge(semi, &s); // Clear white objects.
      break;
    }
    obj = new;
  }
}


static void bench1(int buckets, int len, int iterations) {
  hp* roots;
  int* counts;
  Stats s;
  Nursery ns;
  SemiSpace semi;

  stats_init(&s);
  nursery_init(&ns);
  semi_init(&semi);
  warmup(&ns, &semi);
  roots = calloc(sizeof(hp), buckets);
  counts = calloc(sizeof(int), buckets);
  assert(roots != NULL);
  assert(counts != NULL);

  for(int i=0; i<buckets; i++) {
    hp root = allocate(&ns, &semi, Zero, (MkZero){});
    assert(root != NULL);
    roots[i] = root;
  }

  for(unsigned int i=0; i<iterations; i++) {
    for(int j=buckets-1;j>=0;j--) {
      if( i%(1<<j) == 0 ) {
        hp root=NULL;
        do {
          if( counts[j] == len ) {
            root = allocate(&ns, &semi, Zero, (MkZero){});
            counts[j]=0;
          } else {
            root = allocate(&ns, &semi, Succ, (MkSucc){roots[j]});
          }
          if(root==NULL) {
            stats_nursery_begin(&s);
            for(int n=0;n<buckets;n++) {
              nursery_evacuate(&ns, &semi, &roots[n]);
            }
            nursery_reset(&ns, &s);
            stats_nursery_end(&s);
            if(!semi_check(&semi, NURSERY_SIZE)) {
              semi_scavenge(&semi, &s);
            }
            continue;
          }
          roots[j] = root;
          counts[j]++;
        } while(false);
      }
    }
  }

  semi_close(&semi);
  stats_pprint(&s);
}

static void bench2(int iterations, bool largeObject, bool bypass) {
  Stats s;
  Nursery ns;
  SemiSpace semi;
  hp obj;

  stats_init(&s);
  nursery_init(&ns);
  semi_init(&semi);

  warmup(&ns, &semi);

  obj = allocate(&ns, &semi, Zero, (MkZero){});
  assert(obj!=NULL);

  for(int i=0; i<iterations; i++) {
    hp new = NULL;
    while(new==NULL) {
      if(largeObject)
        new = allocate(&ns, &semi, Branch, (MkBranch){obj, obj});
      else
        new = allocate(&ns, &semi, Succ, (MkSucc){obj});
      if(new==NULL) {
        stats_nursery_begin(&s);
        nursery_evacuate(&ns, &semi, &obj);
        nursery_reset(&ns, &s);
        stats_nursery_end(&s);
        if(!semi_check(&semi, NURSERY_SIZE)) {
          semi_scavenge(&semi, &s);
        }
        if(bypass)
          nursery_bypass(&ns, &semi);
      }
    }
    obj = new;
  }
  semi_close(&semi);
  stats_pprint(&s);
}

static void bench3(int iterations) {
  Stats s;
  Nursery ns;
  SemiSpace semi;
  hp obj;

  stats_init(&s);
  nursery_init(&ns);
  semi_init(&semi);

  for(int i=0; i<iterations; i++) {
    if(allocate(&ns, &semi, Succ, (MkZero){}) == NULL) {
      stats_nursery_begin(&s);
      nursery_reset(&ns, &s);
      stats_nursery_end(&s);

      if(!semi_check(&semi, NURSERY_SIZE)) {
        semi_scavenge(&semi, &s);
      }
    }
  }
  semi_close(&semi);
  stats_pprint(&s);
}

void print_usage(char *prog) {
  printf("Usage: %s {n}\n", prog);
  printf("  n=1   Allocate objects where count*duration is constant.\n");
  printf("  n=2a  Allocate 100%% long-lived objects. 1-Child objects.\n");
  printf("  n=2b  Allocate 100%% long-lived objects. 1-Child objects + nursery bypass.\n");
  printf("  n=2c  Allocate 100%% long-lived objects. 2-Child objects.\n");
  printf("  n=2d  Allocate 100%% long-lived objects. 2-Child objects + nursery bypass.\n");
  printf("  n=3   Allocate 0%% long-lived objects.\n");
}

void enable_lowlatency() {
  struct sched_param param;
  cpu_set_t set;
  CPU_ZERO(&set);
  CPU_SET(sched_getcpu(), &set);
  if(sched_setaffinity(0, sizeof(set), &set) == -1) {
    printf("Failed to set affinity.\n");
  }

  sched_getparam(0, &param);
  param.sched_priority = sched_get_priority_max(SCHED_FIFO);
  if(sched_setscheduler(0, SCHED_FIFO, &param) == -1) {
    printf("Failed to set scheduler.\n");
  }

  if(nice(-19) == -1) {
    printf("Failed to set priority.\n");
  }
}

int main(int argc, char* argv[]) {
  enable_lowlatency();

  if(argc != 2) {
    print_usage(argv[0]);
  } else {
    if(strcmp(argv[1], "1")==0) {
      bench1(20, 10000, 40000000);
    } else if(strcmp(argv[1], "2a")==0) {
      bench2(100000000, false, false);
    } else if(strcmp(argv[1], "2b")==0) {
      bench2(100000000, false, true);
    } else if(strcmp(argv[1], "2c")==0) {
      bench2(100000000, true, false);
    } else if(strcmp(argv[1], "2d")==0) {
      bench2(100000000, true, true);
    } else if(strcmp(argv[1], "3")==0) {
      bench3(1000000000);
    } else {
      print_usage(argv[0]);
    }
  }
  // bench1(15, 10000, 10000000);
  // bench2(100000000);
  // bench3(1000000000);
  return 0;
}
