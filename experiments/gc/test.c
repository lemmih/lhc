#include "common.h"
#include "objects.h"
#include "nursery.h"
#include "semispace.h"
#include "header.h"
#include "stats.h"
#include <stdint.h>
#include <stddef.h>
#include <stdio.h>
#include <assert.h>

#include <criterion/criterion.h>
// #include <criterion/assert.h>
#include <signal.h>


Test(static, sizeof_header) {
  cr_assert(sizeof(Header) == sizeof(word));
}
Test(static, sizeof_infotable) {
  cr_assert(sizeof(InfoTable)/sizeof(ObjectInfo) == TAG_MAX);
}
Test(static, intotable_spotcheck) {
  cr_assert(InfoTable[Leaf].ptrs == 0);
  cr_assert(InfoTable[Leaf].prims == 1);
  cr_assert(InfoTable[Branch].ptrs == 2);
}


Test(nursery, new) {
  Nursery ns;
  nursery_init(&ns);
}
Test(nursery, aroundtrip) {
  Nursery ns;
  hp leaf;
  nursery_init(&ns);

  leaf = allocate(&ns, NULL, Leaf, (MkLeaf){10});
  cr_assert(readHeader(leaf).data.tag == Leaf);
  cr_assert(((MkLeaf*)readObject(leaf))->n == 10);
}
Test(nursery, overflow) {
  // Allocation must fail eventually.
  Nursery ns;
  int i;
  nursery_init(&ns);

  for(i=0;i<NURSERY_SIZE*2;i++) {
    hp leaf = allocate(&ns, NULL, Unit, (MkUnit){});
    if(!leaf)
      break;
  }
  cr_assert(i==NURSERY_SIZE);
}
Test(nursery, allocations) {
  // No early allocation failure.
  Nursery ns;
  nursery_init(&ns);

  for(int i=0;i<NURSERY_SIZE/2;i++) {
    hp leaf = allocate(&ns, NULL, Unit, (MkUnit){});
    cr_assert(leaf);
  }
}
Test(nursery, object_mismatch, .signal=SIGABRT) {
  Nursery ns;
  nursery_init(&ns);

  allocate(&ns, NULL, Leaf, (MkBranch){NULL,NULL});
}
Test(nursery, evacuation) {
  // Nursery evacuation
  Nursery ns;
  SemiSpace semi;
  Stats s;
  hp leaf;
  nursery_init(&ns);
  semi_init(&semi);

  leaf = allocate(&ns, &semi, Leaf, (MkLeaf){10});
  cr_assert_not_null(leaf);
  cr_assert(readHeader(leaf).data.gen == 0); // 0 => nursery

  nursery_begin(&ns, &semi, &s);
  nursery_evacuate(&ns, &semi, &leaf);
  nursery_end(&ns, &semi, &s);

  cr_assert(readHeader(leaf).data.gen == 1);
  cr_assert(!nursery_member(&ns, leaf));
  semi_close(&semi, &s);
}

Test(semispace, shared_object) {
  // Shared object evacuation
  Nursery ns;
  SemiSpace semi;
  Stats s;
  hp leaf, branch;
  nursery_init(&ns);
  semi_init(&semi);

  leaf = allocate(&ns, &semi, Leaf, (MkLeaf){10});
  branch = allocate(&ns, &semi, Branch, (MkBranch){leaf,leaf});

  nursery_begin(&ns, &semi, &s);
  nursery_evacuate(&ns, &semi, &branch);
  nursery_end(&ns, &semi, &s);
  // Check that the leaf object in nursery points forward.
  // cr_assert(readHeader(leaf).data.isForwardPtr == 1);
  // Update leaf reference and check that the new object isn't a forwarding
  // pointer.
  // nursery_evacuate(&ns, &semi, &leaf);
  // cr_assert(readHeader(leaf).data.isForwardPtr == 0);

  // Check that the leaf node hasn't been duplicated.
  cr_assert(((MkBranch*)readObject(branch))->left == ((MkBranch*)readObject(branch))->right);

  semi_close(&semi, &s);
}
Test(semispace, gc) {
  // SemiSpace GC check
  Nursery ns;
  SemiSpace semi;
  Stats s;
  hp leaf;
  nursery_init(&ns);
  semi_init(&semi);

  leaf = allocate(&ns, &semi, Leaf, (MkLeaf){10});
  cr_assert(readHeader(leaf).data.gen == 0);

  nursery_evacuate(&ns, &semi, &leaf);
  cr_assert(readHeader(leaf).data.gen == 1);
  cr_assert(readHeader(leaf).data.grey == 0);
  cr_assert(readHeader(leaf).data.black == semi.black_bit);
  cr_assert(semi_size(&semi) == 2);

  semi_scavenge(&semi, &s);
  cr_assert(readHeader(leaf).data.gen == 1);
  cr_assert(readHeader(leaf).data.grey == 0);
  cr_assert(readHeader(leaf).data.black == !semi.black_bit);
  cr_assert(semi_size(&semi) == 2);

  semi_evacuate(&semi, &leaf);
  cr_assert(readHeader(leaf).data.gen == 1);
  cr_assert(readHeader(leaf).data.grey == 1);
  cr_assert(readHeader(leaf).data.black == !semi.black_bit);
  cr_assert(semi_size(&semi) == 4);

  semi_scavenge(&semi, &s);
  cr_assert(readHeader(leaf).data.gen == 1);
  cr_assert(readHeader(leaf).data.grey == 0);
  cr_assert(readHeader(leaf).data.black == !semi.black_bit);
  cr_assert(semi_size(&semi) == 2);

  semi_close(&semi, &s);
}
Test(semispace, bypass) {
  cr_skip_test();
  Nursery ns;
  SemiSpace semi;
  Stats s;
  hp leaf, prevAddr;
  nursery_init(&ns);
  semi_init(&semi);

  nursery_bypass(&ns, &semi);

  leaf = allocate(&ns, &semi, Leaf, (MkLeaf){10});
  prevAddr = leaf;
  cr_assert(readHeader(leaf).data.gen == 0);

  nursery_begin(&ns, &semi, &s);
  nursery_evacuate(&ns, &semi, &leaf);
  nursery_end(&ns, &semi, &s);
  cr_assert(readHeader(leaf).data.gen == 1);
  cr_assert(readHeader(leaf).data.grey == 0);
  cr_assert(readHeader(leaf).data.black == semi.black_bit);
  cr_assert(leaf == prevAddr);

  semi_close(&semi, &s);
}
Test(semispace, black_allocation) {
  // Newly objects in gen1 /must/ be black.
  // Test:
  //  1. Allocate object, move it to semi, run GC to turn the object white.
  //  2. Allocate pointer to previous object.
  //  3. Move it to semi.
  //  4. Read pointer to make sure it points to a non-white object.
  Nursery ns;
  SemiSpace semi;
  Stats s;
  hp zero, succ;

  nursery_init(&ns);
  semi_init(&semi);

  zero = allocate(&ns, &semi, Zero, (MkZero){});
  nursery_begin(&ns, &semi, &s);
  nursery_evacuate(&ns, &semi, &zero);
  nursery_end(&ns, &semi, &s);
  cr_assert(readHeader(zero).data.gen == 1);
  cr_assert(IS_BLACK(&semi, readHeader(zero)));

  semi_scavenge(&semi, &s);
  cr_assert(IS_WHITE(&semi, readHeader(zero)));

  succ = allocate(&ns, &semi, Succ, (MkSucc){zero});
  nursery_begin(&ns, &semi, &s);
  nursery_evacuate(&ns, &semi, &succ);
  nursery_end(&ns, &semi, &s);
  cr_assert(readHeader(succ).data.gen == 1);
  cr_assert(IS_BLACK(&semi, readHeader(succ)));

  zero = ((MkSucc*)readObject(succ))->next;
  cr_assert(readHeader(zero).data.gen == 1);
  cr_assert(!IS_WHITE(&semi, readHeader(zero)));
}
