#ifndef __LHC_API__
#define __LHC_API__

#include <stdbool.h>
#include <stdint.h>

extern int _lhc_enable_gc_stats;
extern int _lhc_enable_time_stats;
extern int _lhc_enable_tail_copying;
extern int _lhc_enable_tail_compacting;
extern int _lhc_enable_padding;

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

extern const InfoTable _lhc_info_tables[];

typedef uint64_t word;

int _lhc_argc;
char **_lhc_argv;

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

extern int _lhc_main(void);

word _lhc_isIndirection(word);
word _lhc_isIndirectionP(word*);
word* _lhc_getIndirection(word);
word* _lhc_getIndirectionP(word*);
void _lhc_setIndirection(word*, word*);
word _lhc_getTag(word);
word _lhc_mkIndirection(word*);
word _lhc_mkTag(word);

word _lhc_getSize(word header);
word _lhc_setSize(word header, word size);
word _lhc_incSize(word header, word size);
word _lhc_getTail(word header);
word _lhc_setTail(word header, word tail);
word* _lhc_loadLast(word *ptr, word header, word idx);

int _lhc_getargc(void);
char* _lhc_getargv(int n);

#endif // __LHC_API__
