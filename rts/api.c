#include <stdbool.h>
#include "api.h"

bool _lhc_isIndirection(word header) {
  return (header&1) == 1;
}

word *_lhc_getIndirection(word header) {
  return (word*)(header&(~1));
}
word _lhc_getTag(word header) {
  return header>>1;
}

word _lhc_mkIndirection(word *hp) {
  return (word)hp | 1;
}

word _lhc_mkTag(word tag) {
  return tag<<1;
}
