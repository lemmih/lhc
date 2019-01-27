#include "common.h"
#include "utils.h"

void touchSpace(hp ptr, word size) {
  for(int i=0; i < size; i+=512) {
    ptr[i] = i;
  }
}
