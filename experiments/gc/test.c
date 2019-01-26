#include "common.h"
#include "objects.h"
#include "nursery.h"
#include "semispace.h"
#include "header.h"
#include <stdint.h>
#include <stddef.h>
#include <stdio.h>
#include <assert.h>




int main(void) {
  Nursery ns;
  SemiSpace semi;


  { // Header must be exactly one word.
    Header h;
    assert(sizeof(h)==sizeof(word));
  }

  {
    assert(InfoTable[Leaf].ptrs == 0);
    assert(InfoTable[Leaf].prims == 1);
    assert(InfoTable[Branch].ptrs == 2);
  }

  { // Can initialize new nursery.
    nursery_init(&ns);
  }

  { // Allocate/Read round-trip.
    hp leaf;
    nursery_init(&ns);

    leaf = allocate(&ns, Leaf, (MkLeaf){10});
    assert(readHeader(leaf).data.tag == Leaf);
    assert(((MkLeaf*)readObject(leaf))->n == 10);
  }

  { // Allocation must fail eventually.
    int i;
    nursery_init(&ns);

    for(i=0;i<NURSERY_SIZE*2;i++) {
      hp leaf = allocate(&ns, Unit, (MkUnit){});
      if(!leaf)
        break;
    }
    assert(i==NURSERY_SIZE);
  }

  { // No early allocation failure.
    nursery_init(&ns);

    for(int i=0;i<NURSERY_SIZE/2;i++) {
      hp leaf = allocate(&ns, Unit, (MkUnit){});
      assert(leaf);
    }
  }

  { // Nursery evacuation
    hp leaf;
    nursery_init(&ns);
    semi_init(&semi);

    leaf = allocate(&ns, Leaf, (MkLeaf){10});
    assert(leaf != NULL);
    assert(readHeader(leaf).data.gen == 0); // 0 => nursery

    nursery_evacuate(&ns, &semi, &leaf);
    nursery_reset(&ns);
    assert(readHeader(leaf).data.gen == 1);
    assert(!nursery_member(&ns, leaf));
    semi_close(&semi);
  }

  { // Shared object evacuation
    hp leaf, branch;
    nursery_init(&ns);
    semi_init(&semi);

    leaf = allocate(&ns, Leaf, (MkLeaf){10});
    branch = allocate(&ns, Branch, (MkBranch){leaf,leaf});

    nursery_evacuate(&ns, &semi, &branch);
    // Check that the leaf object in nursery points forward.
    assert(readHeader(leaf).data.isForwardPtr == 1);
    // Update leaf reference and check that the new object isn't a forwarding
    // pointer.
    nursery_evacuate(&ns, &semi, &leaf);
    assert(readHeader(leaf).data.isForwardPtr == 0);

    // Check that the leaf node hasn't been duplicated.
    assert(((MkBranch*)readObject(branch))->left == ((MkBranch*)readObject(branch))->right);

    semi_close(&semi);
  }

  { // SemiSpace GC check
    hp leaf;
    nursery_init(&ns);
    semi_init(&semi);

    leaf = allocate(&ns, Leaf, (MkLeaf){10});
    assert(readHeader(leaf).data.gen == 0);

    nursery_evacuate(&ns, &semi, &leaf);
    assert(readHeader(leaf).data.gen == 1);
    assert(readHeader(leaf).data.grey == 0);
    assert(readHeader(leaf).data.black == semi.black_bit);
    assert(semi_size(&semi) == 2);

    semi_scavenge(&semi);
    assert(readHeader(leaf).data.gen == 1);
    assert(readHeader(leaf).data.grey == 0);
    assert(readHeader(leaf).data.black == !semi.black_bit);
    assert(semi_size(&semi) == 2);

    semi_evacuate(&semi, &leaf);
    assert(readHeader(leaf).data.gen == 1);
    assert(readHeader(leaf).data.grey == 1);
    assert(readHeader(leaf).data.black == !semi.black_bit);
    assert(semi_size(&semi) == 4);

    semi_scavenge(&semi);
    assert(readHeader(leaf).data.gen == 1);
    assert(readHeader(leaf).data.grey == 0);
    assert(readHeader(leaf).data.black == !semi.black_bit);
    assert(semi_size(&semi) == 2);

    semi_close(&semi);
  }

  printf("All OK.\n");
  return 0;
}
