#include "lib_shadowstack.h"
#include "nursery.h"
#include "semispace.h"

#define STACK_MAX 1024
static int used=0;
static hp* shadowstack[STACK_MAX];

void ss_push(hp* ref) {
  assert(used < STACK_MAX);
  shadowstack[used] = ref;
  used++;
}
void ss_pop() {
  used--;
}

void ss_mark(Nursery *ns, SemiSpace *semi) {
  for(int i=0;i<used;i++)
    nursery_evacuate(ns, semi, shadowstack[i]);
}
