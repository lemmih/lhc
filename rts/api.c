#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include <string.h>
#include "api.h"
#include "stats.h"

#define SIZE_BITS 12
#define SIZE_MASK ((1<<SIZE_BITS)-1)

// header: -
//         1|indirection|
//         0|size|tail|tag|

word _lhc_isIndirection(word header) {
  return (header&1) == 1;
}
word _lhc_isIndirectionP(word *hp) {
  return _lhc_isIndirection(*hp);
}

word *_lhc_getIndirection(word header) {
  assert(_lhc_isIndirection(header));
  return (word*)(header&(~1));
}
word* _lhc_getIndirectionP(word* hp) {
  return _lhc_getIndirection(*hp);
}

void _lhc_setIndirection(word* hp, word* new) {
  *hp = _lhc_mkIndirection(new);
}

word _lhc_getTag(word header) {
  assert(!_lhc_isIndirection(header));
  return header>>(1+SIZE_BITS+1);
}

word _lhc_mkIndirection(word *hp) {
  return (word)hp | 1;
}

word _lhc_mkTag(word tag) {
  return tag<<(1+SIZE_BITS+1);
}

word _lhc_getSize(word header) {
  assert(!_lhc_isIndirection(header));
  return (header>>1)&SIZE_MASK;
}
word _lhc_setSize(word header, word size) {
  assert(!_lhc_isIndirection(header));
  return (size&SIZE_MASK)<<1 | (header&(~SIZE_MASK));
}
word _lhc_incSize(word header, word size) {
  word oldSize = _lhc_getSize(header);
  assert(oldSize+size < 1<<SIZE_BITS);
  return _lhc_setSize(header, oldSize+size);
}

word _lhc_getTail(word header) {
  assert(!_lhc_isIndirection(header));
  return (header>>(1+SIZE_BITS))&1;
}
word _lhc_setTail(word header, word tail) {
  assert(!_lhc_isIndirection(header));
  return (header&~(1<<(1+SIZE_BITS+1))) | (tail<<(1+SIZE_BITS+1));
}


int _lhc_getargc(void) {
  return _lhc_argc;
}

char* _lhc_getargv(int n) {
  return _lhc_argv[n];
}

void processArgs(int argc, char *argv[]) {
  int rtsMode = 0;
  _lhc_argv = calloc(argc, sizeof(char*));
  _lhc_argc = 0;
  for(int i=0; i < argc; i++) {
    if(strcmp(argv[i], "+RTS")==0 && !rtsMode) {
      rtsMode = 1;
    } else if(strcmp(argv[i], "-RTS")==0 && rtsMode) {
      rtsMode = 0;
    } else if(strcmp(argv[i], "--gc-stats")==0 && rtsMode) {
      _lhc_enable_gc_stats = 1;
    } else if(strcmp(argv[i], "--tail-copy")==0 && rtsMode) {
      _lhc_enable_tail_copying = 1;
    } else if(strcmp(argv[i], "--padding")==0 && rtsMode) {
      _lhc_enable_padding = 1;
    } else if(strcmp(argv[i], "-s")==0 && rtsMode) {
      _lhc_enable_time_stats = 1;
    } else if (rtsMode) {
      fprintf(stderr, "Invalid option: %s\n", argv[i]);
      exit(EXIT_FAILURE);
    } else {
      _lhc_argv[_lhc_argc++] = argv[i];
    }
  }
}

int main(int argc, char *argv[]) {
  _lhc_stats_init();
  setvbuf(stdout, NULL, _IONBF, 0);
  processArgs(argc, argv);
  _lhc_main();
}
