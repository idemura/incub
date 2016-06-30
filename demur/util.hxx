#ifndef IGOR_UTIL_HXX
#define IGOR_UTIL_HXX

#include "base.hxx"
#include <unordered_set>

namespace igor {

template<class T>
struct DerefEqual {
  bool operator()(T *a, T *b) const {
    return *a == *b;
  }
};

template<class T>
struct DerefHash {
  size_t operator()(T *a) const { return std::hash<T>()(*a); }
};

template<class T>
using PtrUnorderedSet = std::unordered_set<T*, DerefHash<T>, DerefEqual<T>>;

}  // namespace

#endif
