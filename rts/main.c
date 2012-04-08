#include "master.h"

extern int showGCInfo;

void __hs_xmain();

void sanity_check(void) {
    if(sizeof(u64) != 8) panic("u64 is not 8 bytes long.");
    if(sizeof(u32) != 4) panic("u32 is not 4 bytes long.");
    if(sizeof(u16) != 2) panic("u16 is not 2 bytes long.");
    if(sizeof(u8) != 1) panic("u8 is not 1 byte long.");
}

int main(int argc, char **argv) {
  /* init */
  sanity_check();
  init_stack();
  init_args(argc, argv);
  //GC_init();

  __hs_xmain(); /* compiler generated code */

  /*if (showGCInfo) {
    fprintf(stderr, "Collections:       %d\n", GC_gc_no);
    fprintf(stderr, "Total allocations: %lu\n", GC_get_total_bytes());
    fprintf(stderr, "Heap size:         %ld\n", GC_get_heap_size());
    }*/

  return 0;
}
