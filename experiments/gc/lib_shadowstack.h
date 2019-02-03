#ifndef __SHADOWSTACK_H__
#define __SHADOWSTACK_H__

#include "common.h"
#include "nursery.h"
#include "semispace.h"

extern int ss_used;
void ss_push(hp* ref);
void ss_pop();
void ss_mark();


#define _ss_allocate(ret, ns, semi, s, t, prims, ptrs, o, size)     \
  do {                                                              \
    do {                                                            \
      ret = _allocate(ns, semi, t, prims, ptrs, o, size);           \
      if(ret==NULL) {                                               \
        stats_timer_begin(s, Gen0Timer);                            \
        ss_mark(ns, semi);                                          \
        nursery_reset(ns, semi, s);                                 \
        if(!semi_check(semi, NURSERY_SIZE)) {                       \
          semi_scavenge(semi, s);                                   \
        }                                                           \
      }                                                             \
    } while (ret==NULL);                                            \
  } while(0)

#define ss_allocate(ret, ns, semi, s, tag, ...) _ss_allocate(ret, ns, semi, s, tag, InfoTable[tag].prims, InfoTable[tag].ptrs, ((Object)(__VA_ARGS__)), sizeof((__VA_ARGS__)))

#endif
