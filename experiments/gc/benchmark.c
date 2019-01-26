#include "common.h"
#include "nursery.h"
#include "semispace.h"
#include "objects.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

void bench1(int buckets, int len, int iterations) {
  hp* roots;
  int* counts;
  Nursery ns;
  SemiSpace semi;

  nursery_init(&ns);
  semi_init(&semi);
  roots = calloc(sizeof(hp), buckets);
  counts = calloc(sizeof(int), buckets);
  assert(roots != NULL);
  assert(counts != NULL);

  for(int i=0; i<buckets; i++) {
    hp root = allocate(&ns, Zero, (MkZero){});
    assert(root != NULL);
    roots[i] = root;
  }

  for(int i=0; i<iterations; i++) {
    // printf("Iteration: %d\n", i);
    for(int j=buckets-1;j>=0;j--) {
      if( i%(1<<j) == 0 ) {
        hp root=NULL;
        while(root==NULL) {
          if( counts[j] == len ) {
            // printf("Bucket overflow: %d\n", j);
            root = allocate(&ns, Zero, (MkZero){});
            counts[j]=0;
          } else {
            root = allocate(&ns, Succ, (MkSucc){roots[j]});
            counts[j]++;
          }
          if(root==NULL) {
            int size=0;
            // printf("GC time! %d\n", i);
            for(int n=0;n<buckets;n++) {
              nursery_evacuate(&ns, &semi, &roots[n]);
              assert(readHeader(roots[n]).data.isForwardPtr == 0);
              assert(readHeader(roots[n]).data.gen == 1);
              // printf("%d ", counts[n]);
              size += counts[n];
            }

            nursery_reset(&ns);
            if(!semi_check(&semi, NURSERY_SIZE)) {
              semi_scavenge(&semi);
              printf("Expected size: %lu kb\n", size*2*sizeof(word)/1024);
              printf("GC Size:       %ld kb\n", semi_size(&semi)*sizeof(word)/1024);
              // for(int n=0;n<buckets;n++) {
              //   nursery_evacuate(&ns, &semi, &roots[n]);
              // }
              // semi_scavenge(&semi);
              // printf("GC Final size: %ld kb\n", semi_size(&semi)*sizeof(word)/1024);
            }
          } else {
            assert(readHeader(root).data.isForwardPtr == 0);
            assert(readHeader(root).data.gen == 0);

            roots[j] = root;
          }
        }
      }
    }
  }

  semi_close(&semi);
}

int main() {
  bench1(15, 10000, 10000000);
  return 0;
}
