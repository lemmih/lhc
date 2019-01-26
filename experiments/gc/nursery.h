#ifndef __NURSERY_H__
#define __NURSERY_H__

#include "header.h"
#include "objects.h"
#include "semispace.h"
#include <stdbool.h>

#define NURSERY_SIZE (1024*512/sizeof(word))

typedef struct {
  word heap[NURSERY_SIZE];
  hp index;
  unsigned int evacuated;
} Nursery;

void nursery_init(Nursery *);

#define allocate(ns, tag, ...) _allocate(ns, tag, ((Object)(__VA_ARGS__)), sizeof((__VA_ARGS__)))
hp _allocate(Nursery *ns, enum Tag t, Object o, unsigned long size);

void nursery_evacuate(Nursery *ns, SemiSpace *semi, hp* objAddr);
void nursery_reset(Nursery *ns);
bool nursery_member(Nursery *ns, hp obj);

Header readHeader(hp ptr);
Object* readObject(hp ptr);

#endif
