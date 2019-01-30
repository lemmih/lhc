#ifndef __SEMISPACE_H__
#define __SEMISPACE_H__

#include "common.h"
#include "stats.h"
#include <stdbool.h>
#include <assert.h>

#define MIN_AREA (1024*1024/sizeof(word))

typedef struct {
  unsigned int black_bit:1;
  double factor;
  bool has_roots;

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
void semi_close(SemiSpace *semi, Stats*);

// hp semi_bump_grey(SemiSpace *semi, word size);
// hp semi_bump_black(SemiSpace *semi, word size);
bool semi_check(SemiSpace *semi, word size);

void semi_evacuate(SemiSpace *semi, hp* objAddr);
void semi_scavenge(SemiSpace *semi, Stats*);
void semi_scavenge_concurrent(SemiSpace *semi, Stats *s);
word semi_size(SemiSpace *semi);

#define IS_WHITE(semi, header) (!(IS_BLACK(semi,header) || IS_GREY(header)))
#define IS_BLACK(semi, header) (header.data.black == semi->black_bit)
#define IS_GREY(header) (header.data.grey)

static hp semi_bump_black(SemiSpace *semi, word size) {
  hp ret;
  assert(semi->black_space.free+size <= area_limit(semi->black_space));
  ret = semi->black_space.free;
  semi->black_space.free += size;
  return ret;
}

static hp semi_bump_grey(SemiSpace *semi, word size) {
  hp ret;
  assert(semi->grey_space.free+size <= area_limit(semi->grey_space));
  ret = semi->grey_space.free;
  semi->grey_space.free += size;
  return ret;
}




#endif
