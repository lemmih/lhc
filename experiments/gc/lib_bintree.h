#ifndef __BINTREE_H__
#define __BINTREE_H__

#include "common.h"
#include "nursery.h"
#include "semispace.h"

void bintree_print(hp tree);
hp bintree_new(Nursery *ns, SemiSpace *semi, Stats *s);
hp bintree_insert(Nursery *ns, SemiSpace *semi, Stats *s, hp tree, word value);

#endif
