#include <stdint.h>

/*
Activation records:
1: ptr to return function. This function has a prefix
2: ptr to previous record. May be null.
3: primitives
4: heap pointers
*/
typedef struct {
  unsigned int recordSize;
  unsigned int nPrimitives;
  unsigned int nHeapPointers;
} ActivationInfo;

typedef struct {
  unsigned int nPrimitives;
  unsigned int nHeapPointers;
} InfoTable;

extern InfoTable _lhc_info_tables[];

typedef uint64_t word;

// typedef union {
//   struct Node {
//     int type:2;
//     int tag:16;
//     int flags:14;
//     int trunk:32;
//   }
//   union Pointer {
//     int type:2
//     void* child;
//   }
// } Header;

// Object layout:
//   header: 1 word
//   primitives: n words
//   pointers: i words
// Header:
//   type: 2 bits
//     00: Stack frame
//     01: Indirection
//     10: Ordinary object
//   tag: 16 bits
//   flags: 14 bits
//   trunk: 32 bits // only on 64 bit systems.
#define OBJECT_HEADER(hp) (*hp)
