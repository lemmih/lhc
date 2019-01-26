#ifndef __SEMISPACE_H__
#define __SEMISPACE_H__

#include "common.h"
#include <stdbool.h>

#define MIN_AREA (1024*1024/sizeof(word))

typedef struct {
  unsigned int black_bit:1;
  double factor;

  struct {
    hp ptr1;
    hp ptr2;
    int size;
  } white_space;

  struct {
    hp ptr;
    hp free;
    hp scavenged;
    int size;
  } grey_space;

  struct {
    hp ptr;
    hp free;
    int size;
  } black_space;
} SemiSpace;

#define area_limit(area) (area.ptr+area.size)
#define area_used(area) (area.free-area.ptr)
void semi_init(SemiSpace *semi);
void semi_close(SemiSpace *semi);

hp semi_bump_grey(SemiSpace *semi, word size);
hp semi_bump_black(SemiSpace *semi, word size);
bool semi_check(SemiSpace *semi, word size);

void semi_evacuate(SemiSpace *semi, hp* objAddr);
void semi_scavenge(SemiSpace *semi);
word semi_size(SemiSpace *semi);

#endif
