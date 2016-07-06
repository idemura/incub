#ifndef IGOR_UTIL_HXX
#define IGOR_UTIL_HXX

#include "base.hxx"
#include <unordered_set>

namespace igor {

template<typename T>
struct DerefEqual {
  bool operator()(T *a, T *b) const {
    return *a == *b;
  }
};

template<typename T>
struct DerefHash {
  size_t operator()(T *a) const { return std::hash<T>()(*a); }
};

template<typename T>
using PtrUnorderedSet = std::unordered_set<T*, DerefHash<T>, DerefEqual<T>>;

}  // namespace

#endif
