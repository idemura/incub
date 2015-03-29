#include <algorithm>
#include <functional>
#include <map>
#include <string>
#include <vector>
#include <utility>
#include <math.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define INF 0x7fffffff

typedef long long int lli;

// Min heap for value type non-exceptional type T.
template<class T, class Cmp = std::less<T>>
class MinHeap {
public:
  MinHeap() {}

  void push(T v) {
    h.push_back(v);
    std::push_heap(h.begin(), h.end(), cmp);
  }

  bool empty() const { return h.empty(); }
  T top() const { return h[0]; }

  T pop_min() {
    std::pop_heap(h.begin(), h.end(), cmp);
    T min = h.back();
    h.pop_back();
    return min;
  }

private:
  struct SwapCmp {
    bool operator()(T a, T b) const { return c(b, a); }
    Cmp c;
  } cmp;
  std::vector<T> h;
};

template<class T, class Cmp = std::less<T>>
class Heap {
public:
  typedef T ValueT;

  explicit Heap(const Cmp &cmp = Cmp()): cmp(cmp) {}

  void insert(const T &val, size_t *index = NULL) {
    h.push_back(Elem(val, index));
    setIndex(h.size() - 1);
    heapifyParent(h.size() - 1);
    check();
  }

  // If you changed priority (either decrease or increase call this to
  // restore heap property.
  void reheapAt(size_t i) {
    if (i > 0 && !less((i - 1) / 2, 0)) {
      heapifyParent(i);
    } else {
      for (;;) {
        size_t kmin = i, k;
        k = 2 * i + 1;
        if (k < h.size() && less(k, kmin)) {
          kmin = k;
        }
        k = 2 * i + 2;
        if (k < h.size() && less(k, kmin)) {
          kmin = k;
        }
        if (kmin != i) {
          std::swap(h[kmin], h[i]);
          setIndex(i);
          setIndex(kmin);
          i = kmin;
        } else {
          break;
        }
      }
    }
    // check();
  }

  T popMin() {
    return remove(0);
  }

  T remove(size_t i) {
    // Swap i-th and last and restore heap property (heapify). If the last
    // element was less than i-th, restore up, else - restore down.
    assert(i < h.size());
    h[i].setIndex(kNoIndex);
    const T val = h[i].val;
    h[i] = h[h.size() - 1];
    setIndex(i);
    h.pop_back();
    if (h.size() > 1) {
      reheapAt(i);
    }
    return val;
  }

  size_t size() const {
    return h.size();
  }

private:
  struct Elem {
    T val;
    size_t *index;

    Elem(const T& val, size_t *index): val(val), index(index) {
      setIndex(kNoIndex);
    }

    void setIndex(size_t i) {
      if (index) *index = i;
    }
  };

  void setIndex(size_t i) {
    h[i].setIndex(i);
  }

  bool less(size_t i, size_t j) const {
    return cmp(h[i].val, h[j].val);
  }

  void heapifyParent(size_t i) {
    for (; i > 0;) {
      size_t p = (i - 1) / 2;
      if (less(p, i)) {
        break;
      }
      std::swap(h[p], h[i]);
      setIndex(p);
      setIndex(i);
      i = p;
    }
  }

  void check() const {
    for (size_t i = 1; i < h.size(); i++) {
      size_t p = (i - 1) / 2;
      if (less(i, p)) {
        fprintf(stderr, "*** Heap corrupted at %zu (parent %zu)\n", i, p);
      }
    }
  }

  static const size_t kNoIndex = -1;
  Cmp cmp;
  std::vector<Elem> h;
};

void testHeapRemove(const int *a, int an, int rmi)
{
  auto *ind = new size_t[an]();
  Heap<int> heap;
  for (int i = 0; i < an; i++) {
    heap.insert(a[i], ind + i);
  }
  heap.remove(ind[rmi]);
  // `popMin` will produce a sorted sequence.
  int prev = heap.popMin();
  for (; heap.size() > 0;) {
    int x = heap.popMin();
    assert(prev <= x);
    prev = x;
  }
  printf("Test case %d OK.\n", rmi);
}

int main(int argc, char **argv)
{
// #ifndef ONLINE_JUDGE
//   freopen("in", "r", stdin);
// #endif
  const int a[] = {9, 5, 1, 7, 8, 2};
  const int an = ARRAY_SIZEOF(a);
  for (int i = 0; i < an; i++) {
    testHeapRemove(a, an, i);
  }
  return 0;
}
