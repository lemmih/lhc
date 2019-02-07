#define _GNU_SOURCE
#include "common.h"
#include "nursery.h"
#include "semispace.h"
#include "objects.h"
#include "stats.h"
#include "lib_bintree.h"
#include "lib_shadowstack.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <unistd.h>
#include <errno.h>
#include <sched.h>
#include <x86intrin.h>

static void warmup(Nursery *ns, SemiSpace *semi) {
  Stats s;
  hp obj, new=NULL;
  stats_init(&s);
  obj = allocate(ns, semi, Zero, (MkZero){});
  assert(obj!=NULL);

  for(;;) {
    new = allocate(ns, semi, Branch, (MkBranch){obj, obj});
    if(new==NULL) {
      // printf("Nursery full\n");
      nursery_begin(ns, semi, &s);
      nursery_evacuate(ns, semi, &obj);
      nursery_end(ns, semi, &s);
      semi_scavenge(semi, &s); // Turn black objects white.
      semi_scavenge(semi, &s); // Clear white objects.
      break;
    }
    obj = new;
  }
}


static void bench1(Stats *s, const int buckets, const int len, const int iterations) {
  hp* roots;
  int* counts;
  Nursery ns;
  SemiSpace semi;


  nursery_init(&ns);
  semi_init(&semi);
  warmup(&ns, &semi);
  roots = calloc(sizeof(hp), buckets);
  counts = calloc(sizeof(int), buckets);
  assert(roots != NULL);
  assert(counts != NULL);

  stats_timer_begin(s, MutTimer);

  for(int i=0; i<buckets; i++) {
    hp root = allocate(&ns, &semi, Zero, (MkZero){});
    assert(root != NULL);
    roots[i] = root;
  }

  int high=0;
  word sum=0;

  for(unsigned int i=0; i<iterations; i++) {
    for(int j=0;j<buckets && (i%(1<<j) == 0);j++) {
      hp root=NULL;
      do {
        if( counts[j] == len*(j+1) ) {
          root = allocate(&ns, &semi, Zero, (MkZero){});
          sum-=counts[j];
          counts[j]=0;
        } else {
          root = allocate(&ns, &semi, Succ, (MkSucc){roots[j]});
        }
        if(root==NULL) {

          // if(sum>high) {
          //   printf("Sum: %lu KB\t\t\r", sum*2*8/1024);
          //   high = sum;
          //   fflush(stdout);
          // }

          nursery_begin(&ns, &semi, s);
          for(int n=0;n<buckets;n++) {
            nursery_evacuate(&ns, &semi, &roots[n]);
          }
          nursery_end(&ns, &semi, s);
          semi_scavenge_concurrent(&semi, s);
          if(!semi_check(&semi, NURSERY_SIZE)) {
            semi_scavenge(&semi, s);
          }
          continue;
        }
        roots[j] = root;
        counts[j]++;
        sum++;
      } while(false);
    }
  }
  // printf("\n");
  stats_timer_end(s);


  semi_close(&semi, s);
}

static void bench2(Stats *s, const int iterations, const bool largeObject, const bool bypass) {
  Nursery ns;
  SemiSpace semi;
  hp obj;
  register hp index;
  register hp limit;

  nursery_init(&ns);
  semi_init(&semi);

  warmup(&ns, &semi);
  stats_timer_begin(s, MutTimer);

  obj = allocate(&ns, &semi, Zero, (MkZero){});
  assert(obj!=NULL);

  index = ns.index;
  limit = ns.limit;

  for(int i=0; i<iterations; i++) {
    hp new = NULL;
    while(new==NULL) {
      if(largeObject) {
        // new = allocate(&ns, &semi, Branch, (MkBranch){obj, obj});
        {
          Header header;

          if(limit < index+(2+1)) {
            new = NULL;
          } else {
            new = index;
            Object o = (Object)((MkBranch){obj, obj});

            header.raw = 0;
            header.data.tag = Branch;
            header.data.prims = 0;
            header.data.ptrs = 2;
            *index = header.raw;
            index++;
            memcpy(index, &o, 16);
            index += 2;
          }
        }
      } else {
        // new = allocate(&ns, &semi, Succ, (MkSucc){obj});
        {
          Header header;

          if(limit < index+(1+1)) {
            new = NULL;
          } else {
            new = index;
            Object o = (Object)((MkSucc){obj});

            header.raw = 0;
            header.data.tag = Succ;
            header.data.prims = 0;
            header.data.ptrs = 1;
            *index = header.raw;
            index++;
            memcpy(index, &o, 8);
            index += 1;
          }
        }
      }
      if(new==NULL) {
        ns.index = index;
        nursery_begin(&ns, &semi, s);
        nursery_evacuate(&ns, &semi, &obj);
        nursery_end(&ns, &semi, s);
        semi_scavenge_concurrent(&semi, s);
        if(!semi_check(&semi, NURSERY_SIZE)) {
          semi_scavenge(&semi, s);
        }
        if(bypass)
          nursery_bypass(&ns, &semi);
        index = ns.index;
        limit = ns.limit;
      }
    }
    obj = new;
  }
  stats_timer_end(s);
  semi_close(&semi, s);
}

static void bench3(Stats *s, const long int iterations, const int objType) {
  Nursery ns;
  SemiSpace semi;
  hp obj;
  register hp index;
  register hp limit;

  nursery_init(&ns);
  semi_init(&semi);
  warmup(&ns, &semi);
  stats_timer_begin(s, MutTimer);

  index = ns.index;
  limit = ns.limit;

  for(long int i=0; i<iterations; i++) {
    if(objType==2) {
      // obj = allocate(&ns, &semi, IntBranch, (MkIntBranch){0, (hp)0, (hp)0});
      {
        Header header;
        if(index+2*(3+1) >= limit) {
          obj = NULL;
        } else {
          obj = index;

          header.raw = 0;
          header.data.tag = IntBranch;
          header.data.prims = 1;
          header.data.ptrs = 2;
          *index = header.raw;
          index++;
          index[0] = 0xDEADBEEF;
          index[1] = 0xBEEF;
          index[2] = 0xDEAD;
          index += 3;
        }
      }
    } else if(objType==1) {
      // obj = allocate(&ns, &semi, IntBranch, (MkIntBranch){i, (hp)i, (hp)i});
      {
        Header header;
        if(index+(3+1) >= limit) {
          obj = NULL;
        } else {
          obj = index;
          Object o = (Object)((MkIntBranch){i, (hp)i, (hp)i});

          header.raw = 0;
          header.data.tag = IntBranch;
          header.data.prims = 1;
          header.data.ptrs = 2;
          *index = header.raw;
          index++;
          memcpy(index, &o, 24);
          index += 3;
        }
      }
    } else if(objType==0) {
      // obj = allocate(&ns, &semi, Unit, (MkUnit){});
      {
        Header header;

        if(limit < index+(0+1)) {
          obj = NULL;
        } else {
          obj = index;
          Object o = (Object)((MkUnit){});

          header.raw = 0;
          header.data.tag = Unit;
          header.data.prims = 0;
          header.data.ptrs = 0;
          *index = header.raw;
          index++;
          memcpy(index, &o, 0);
          index += 0;
        }
      }
    }
    if(obj == NULL) {
      // index = ns.heap;
      ns.index = index;
      nursery_begin(&ns, &semi, s);
      nursery_end(&ns, &semi, s);
      semi_scavenge_concurrent(&semi, s);
      if(!semi_check(&semi, NURSERY_SIZE)) {
        semi_scavenge(&semi, s);
      }
      index = ns.index;
      limit = ns.limit;
    }
  }
  stats_timer_end(s);
  semi_close(&semi, s);
}

void bench4(Stats *s, int iterations) {
  Nursery ns;
  SemiSpace semi;
  hp tree;

  srandom(0xdeadbeef);

  nursery_init(&ns);
  semi_init(&semi);
  warmup(&ns, &semi);
  stats_timer_begin(s, MutTimer);

  tree = bintree_new(&ns, &semi, s);
  ss_push(&tree);
  for(int i=0; i<iterations; i++)
    tree = bintree_insert(&ns, &semi, s, tree, random());

  // bintree_print(tree);
  ss_pop();

  stats_timer_end(s);
  semi_close(&semi, s);
}

// static void bench4(int iterations) {
//
// }

void print_usage(char *prog) {
  printf("Usage: %s {n}\n", prog);
  printf("  n=0   Run all benchmarks.\n");
  printf("  n=1   Allocate objects where count*duration is constant.\n");
  printf("  n=2a  Allocate 100%% long-lived objects. 1-Child objects.\n");
  printf("  n=2b  Allocate 100%% long-lived objects. 1-Child objects + nursery bypass.\n");
  printf("  n=2c  Allocate 100%% long-lived objects. 2-Child objects.\n");
  printf("  n=2d  Allocate 100%% long-lived objects. 2-Child objects + nursery bypass.\n");
  printf("  n=3a   Allocate 0%% long-lived objects. Small objects.\n");
  printf("  n=3b   Allocate 0%% long-lived objects. Large objects.\n");
  printf("  n=3c   Allocate 0%% long-lived objects. Large objects. Constant data.\n");
  printf("  n=4   Insert into binary tree.\n");
}

// OK:  0 1 2 3 4 6
// BAD: 6 4 3
void enable_lowlatency() {
  struct sched_param param;
  cpu_set_t set;
  CPU_ZERO(&set);
  CPU_SET(sched_getcpu(), &set);
  if(sched_setaffinity(0, sizeof(set), &set) == -1) {
    // printf("Failed to set affinity.\n");
  }

  sched_getparam(0, &param);
  param.sched_priority = sched_get_priority_max(SCHED_FIFO);
  if(sched_setscheduler(0, SCHED_FIFO, &param) == -1) {
    // printf("Failed to set scheduler.\n");
  }

  if(nice(-19) == -1) {
    // printf("Failed to set priority.\n");
  }
}

int main(int argc, char* argv[]) {
  Stats s;
  stats_init(&s);
  enable_lowlatency();

  if(argc != 2) {
    print_usage(argv[0]);
    return -1;
  } else {
    if(strcmp(argv[1], "0")==0) {
      bench1(&s, 20, 10000, 100000000);
      bench2(&s, 50000000, false, false);
      bench2(&s, 50000000, true, false);
      bench3(&s, 3000000000, 0);
      bench3(&s, 3000000000, 1);
      bench3(&s, 3000000000, 2);
      bench4(&s, 3000000);
    } else if(strcmp(argv[1], "1")==0) {
      bench1(&s, 20, 10000, 100000000);
    } else if(strcmp(argv[1], "2a")==0) {
      bench2(&s, 50000000, false, false);
    } else if(strcmp(argv[1], "2b")==0) {
      bench2(&s, 50000000, false, true);
    } else if(strcmp(argv[1], "2c")==0) {
      bench2(&s, 50000000, true, false);
    } else if(strcmp(argv[1], "2d")==0) {
      bench2(&s, 50000000, true, true);
    } else if(strcmp(argv[1], "3a")==0) {
      bench3(&s, 3000000000, 0);
    } else if(strcmp(argv[1], "3b")==0) {
      bench3(&s, 3000000000, 1);
    } else if(strcmp(argv[1], "3c")==0) {
      bench3(&s, 3000000000, 2);
    } else if(strcmp(argv[1], "4")==0) {
      bench4(&s, 3000000);
    } else {
      print_usage(argv[0]);
      return -1;
    }
  }
  stats_pprint(&s);
  return 0;
}
