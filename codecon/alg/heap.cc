#include <algorithm>
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
#define INF 0x3fffffff

typedef long long int lli;

template<class T>
class OperatorLess {
 public:
  bool operator()(const T &l, const T &r) const
  {
    return l < r;
  }
};

template<class T, class Cmp = OperatorLess<T> >
class MinHeap {
 public:
  explicit MinHeap(const Cmp &cmp = Cmp()): cmp(cmp) {}

  void insert(T val)
  {
    v.push_back(val);
    for (size_t i = v.size() - 1; i > 0;) {
      size_t p = (i - 1) / 2;
      if (cmp(v[p], v[i])) {
        break;
      }
      std::swap(v[p], v[i]);
      i = p;
    }
    // check();
  }

  T popMin()
  {
    assert(v.size() > 0);
    const T min_val = v[0];
    v[0] = v[v.size() - 1];
    v.pop_back();
    for (size_t i = 0; i < v.size();) {
      size_t jmin = i, k;
      k = 2 * i + 1;
      if (k < v.size() && cmp(v[k], v[jmin])) {
        jmin = k;
      }
      k = 2 * i + 2;
      if (k < v.size() && cmp(v[k], v[jmin])) {
        jmin = k;
      }
      if (jmin == i) {
        break;
      }
      std::swap(v[i], v[jmin]);
      i = jmin;
    }
    // check();
    return min_val;
  }

  size_t size() const
  {
    return v.size();
  }

  void check() const
  {
    for (size_t i = 1; i < v.size(); i++) {
      size_t p = (i - 1) / 2;
      if (cmp(v[i], v[p])) {
        printf("*** Heap corrupted at %zu (parent %zu)\n", i, p);
      }
    }
  }

 private:
  Cmp cmp;
  std::vector<T> v;
};

int main(int argc, char **argv)
{
// #ifndef ONLINE_JUDGE
//   freopen("in", "r", stdin);
// #endif
  MinHeap<int> h;
  h.insert(9);
  h.insert(5);
  h.insert(1);
  h.insert(7);
  h.insert(8);
  h.insert(2);
  assert(h.popMin() == 1);
  assert(h.popMin() == 2);
  assert(h.popMin() == 5);
  assert(h.popMin() == 7);
  assert(h.popMin() == 8);
  assert(h.popMin() == 9);
  printf("OK.\n");
  return 0;
}
