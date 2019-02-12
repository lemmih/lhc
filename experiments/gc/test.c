#include "common.h"
#include "objects.h"
#include "nursery.h"
#include "semispace.h"
#include "marksweep.h"
#include "header.h"
#include "stats.h"
#include <stdint.h>
#include <stddef.h>
#include <stdio.h>
#include <assert.h>

#include <criterion/criterion.h>
// #include <criterion/assert.h>
#include <signal.h>

#define SETUP \
  Nursery ns;\
  SemiSpace semi;\
  Stats s;\
  nursery_init(&ns);\
  semi_init(&semi)

Test(basic, mmap_aligned) {
  void* p;
  p = mmap_aligned(MS_BLOCK_SIZE, MS_BLOCK_SIZE, PROT_READ|PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE);
  cr_assert_not_null(p);
  cr_assert(GET_BLOCK_PTR(p) == p);
}
Test(static, sizeof_header) {
  cr_assert(sizeof(Header) == sizeof(word));
}
Test(static, sizeof_infotable) {
  cr_assert(sizeof(InfoTable)/sizeof(ObjectInfo) == TAG_MAX);
}
Test(static, valid_infotable) {
  bool seen[TAG_MAX];
  for(int i=0; i<TAG_MAX; i++) {
    seen[i] = false;
  }
  for(int i=0; i<TAG_MAX; i++) {
    cr_assert(seen[InfoTable[i].tag] == false, "Duplicate ObjectInfo for %d", InfoTable[i].tag);
    seen[InfoTable[i].tag] = true;
  }
  for(int i=0; i<TAG_MAX; i++) {
    cr_assert(seen[i] == true, "Missing ObjectInfo for %d", i);
  }
}
Test(static, intotable_spotcheck) {
  cr_assert(InfoTable[Leaf].ptrs == 0);
  cr_assert(InfoTable[Leaf].prims == 1);
  cr_assert(InfoTable[Branch].ptrs == 2);
  cr_assert(!InfoTable[Branch].mutable);
  cr_assert(!InfoTable[Leaf].mutable);
  cr_assert(InfoTable[IORef].mutable);
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
  SETUP;
  hp leaf;

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
  SETUP;
  hp leaf, branch;

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
  SETUP;
  hp leaf;

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
// Bypass
// Objects allocated in nurseries are not black. They're turned black
// by the stop-the-world GC when they're moved into gen1.
// Bypass means newly allocated objects are directly allocated in gen1
// and therefore have to be black.
// 1. Test the black bit.
// 2. Test that white references are turned grey.
Test(semispace, bypass) {
  // cr_skip_test(); // bypass is too expensive to be enabled right now.
  SETUP;
  hp leaf, succ;

  nursery_bypass(&ns, &semi);

  leaf = allocate(&ns, &semi, Leaf, (MkLeaf){10});
  cr_assert(readHeader(leaf).data.gen == 1);
  cr_assert(IS_BLACK(&semi, readHeader(leaf)));

  semi_scavenge(&semi, &s); // Flip turn black objects white.
  nursery_bypass(&ns, &semi); // Reserve a new block in gen1 for allocations.
  cr_assert(IS_WHITE(&semi, readHeader(leaf)));

  succ = allocate(&ns, &semi, Succ, (MkSucc){leaf});
  cr_assert(readHeader(succ).data.gen == 1);
  cr_assert(IS_BLACK(&semi, readHeader(succ)));

  cr_assert(readHeader(leaf).data.isForwardPtr);
  followIndirection(&leaf);
  cr_assert(IS_GREY(readHeader(leaf)));
  cr_assert(!IS_BLACK(&semi, readHeader(leaf)));

  semi_close(&semi, &s);
}
Test(semispace, black_allocation) {
  // Newly objects in gen1 /must/ be black.
  // Test:
  //  1. Allocate object, move it to semi, run GC to turn the object white.
  //  2. Allocate pointer to previous object.
  //  3. Move it to semi.
  //  4. Read pointer to make sure it points to a non-white object.
  SETUP;
  hp zero, succ;

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
Test(semispace, cycle) {
  // Test evacuation of cyclic data.
  SETUP;
  hp rec;

  rec = allocate(&ns, &semi, Succ, (MkSucc){NULL});
  ((MkSucc*)readObject(rec))->next = rec;
  cr_assert(readHeader(rec).data.gen == 0);

  nursery_begin(&ns, &semi, &s);
  nursery_evacuate(&ns, &semi, &rec);
  nursery_end(&ns, &semi, &s);

  cr_assert(readHeader(rec).data.gen == 1);
  cr_assert(((MkSucc*)readObject(rec))->next == rec);
}
Test(nursery, bad_ref) {
  // Test evacuation of cyclic data.
  SETUP;
  hp rec;

  rec = allocate(&ns, &semi, Succ, (MkSucc){NULL});
  cr_assert(readHeader(rec).data.gen == 0);

  // The nursery doesn't touch pointers outside of the nursery so the NULL
  // pointer doesn't do any harm.
  nursery_begin(&ns, &semi, &s);
  nursery_evacuate(&ns, &semi, &rec);
  nursery_end(&ns, &semi, &s);

  cr_assert(readHeader(rec).data.gen == 1);
  cr_assert(((MkSucc*)readObject(rec))->next == NULL);
}
Test(semispace, bad_ref, .signal = SIGSEGV) {
  // Test evacuation of cyclic data.
  SETUP;
  hp rec;

  rec = allocate(&ns, &semi, Succ, (MkSucc){NULL});

  nursery_begin(&ns, &semi, &s);
  nursery_evacuate(&ns, &semi, &rec);
  nursery_end(&ns, &semi, &s);

  semi_scavenge(&semi, &s);
  nursery_begin(&ns, &semi, &s);
  nursery_evacuate(&ns, &semi, &rec);
  nursery_end(&ns, &semi, &s);
  semi_scavenge(&semi, &s); // This should cause a segfault.
  cr_assert_fail("Expected segfault.");
}

Test(marksweep, block_size) {
  Block b;
  MarkStack s;
  cr_assert(sizeof(Block) == MS_BLOCK_SIZE);
  cr_assert(sizeof(MarkStack) == MS_BLOCK_SIZE);
  // cr_assert(MS_CHUNK_SIZE == 8);
  cr_assert(sizeof(b.data)/sizeof(*b.data) == MS_BLOCK_ENTRIES);
  cr_assert(sizeof(s.stack)/sizeof(*s.stack) == MS_STACK_ENTRIES);
}
Test(marksweep, reserve_block) {
  MarkSweep ms;
  Block *block;
  // cr_skip_test();

  ms_init(&ms);
  block = ms_reserve_block(&ms);
  cr_assert_not_null(block);
  cr_assert(ms_block_popcount(block) == 0);

  ms_mark_object(0,&block->data[0]);
  ms_mark_object(0,&block->data[MS_BLOCK_ENTRIES-1]);
}
Test(marksweep, push_pop) {
  MarkSweep ms;
  ms_init(&ms);

  ms_stack_push(&ms, (hp*) NULL);
  cr_assert_null(ms_stack_pop(&ms));
  // cr_assert(ms)
}
Test(marksweep, sweep_empty) {
  MarkSweep ms;
  ms_init(&ms);

  Block *block = ms_reserve_block(&ms);



  Header header;
  header.data.tag = Unit;
  header.data.gen = 2;
  header.data.prims = InfoTable[Unit].prims;
  header.data.ptrs = InfoTable[Unit].ptrs;
  header.data.black = ms.black_bit;
  block->data[0] = header.raw;
  block->data[1] = header.raw;
  block->data[2] = header.raw;

  ms_mark_all(&ms);

  hp ref1 = &block->data[0];
  ms_stack_push(&ms, &ref1);
  // hp ref2 = &block->data[1];
  // ms_stack_push(&ms, &ref2);
  // hp ref3 = &block->data[2];
  // ms_stack_push(&ms, &ref3);

  ms_mark_all(&ms);
}
