#include "lib_bintree.h"
#include "lib_shadowstack.h"
#include "common.h"
#include "objects.h"
#include "nursery.h"
#include "semispace.h"

#include <stdbool.h>

hp bintree_new(Nursery *ns, SemiSpace *semi, Stats *s) {
  hp unit;
  ss_allocate(unit, ns, semi, s, Unit, (MkUnit){});
  assert(unit != NULL);
  return unit;
}

hp bintree_insert(Nursery *ns, SemiSpace *semi, Stats *s, hp tree, word value) {
  Header header;
  hp ret;

  header = readHeader(tree);
  while( header.data.isForwardPtr ) {
    tree = (hp) ((word)header.forwardPtr & (~1));
    header = readHeader(tree);
  }

  switch( header.data.tag) {
    case Unit:
      ss_push(&tree);
      ss_allocate(ret, ns, semi, s, IntBranch, (MkIntBranch){value, tree, tree});
      ss_pop();
      return ret;
    case IntBranch: {
      MkIntBranch* branch = (MkIntBranch*)readObject(tree);
      int n = branch->n;
      hp left = branch->left;
      hp right = branch->right;
      if( n > value ) {
        // Insert to the left.
        ss_push(&right);
        hp subtree = bintree_insert(ns, semi, s, left, value);
        ss_push(&subtree);
        ss_allocate(ret, ns, semi, s, IntBranch, (MkIntBranch){n, subtree, right});
        ss_pop();
        ss_pop();
        return ret;
      } else if( n < value ) {
        ss_push(&left);
        hp subtree = bintree_insert(ns, semi, s, right, value);
        ss_push(&subtree);
        ss_allocate(ret, ns, semi, s, IntBranch, (MkIntBranch){n, left, subtree});
        ss_pop();
        ss_pop();
        return ret;
      } else {
        // Already in tree. Just return.
        return tree;
      }
    }
    default:
      abort();
  }
}
/*
Unit:
-

Branch 10 Unit unit
10

Branch 10 (Branch 5 - -) (Branch 15 - -)
10
|- 5
|- 15

B 10 (B 5 (B 2 - -) (B 7 - -)) (Branch 15 - -)
10
|- 5
||- 2
||- 7
|- 15
*/
void bintree_print_n(hp tree, int depth) {
  Header header;

  header = readHeader(tree);
  while( header.data.isForwardPtr ) {
    tree = (hp) ((word)header.forwardPtr & (~1));
    header = readHeader(tree);
  }

  switch( header.data.tag) {
    case Unit:
      break;
    case IntBranch: {
      MkIntBranch* branch = (MkIntBranch*)readObject(tree);
      int n = branch->n;
      hp left = branch->left;
      hp right = branch->right;
      for(int i=0; i < depth; i++) {
        printf("|");
      }
      printf("- %d\n", n);
      bintree_print_n(left, depth+1);
      bintree_print_n(right, depth+1);
      break;
    }
    default:
      abort();
  }
}

void bintree_print(hp tree) {
  bintree_print_n(tree, 0);
}
