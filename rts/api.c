#include <stdbool.h>
#include <assert.h>
#include "api.h"

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
  return header>>1;
}

word _lhc_mkIndirection(word *hp) {
  return (word)hp | 1;
}

word _lhc_mkTag(word tag) {
  return tag<<1;
}
