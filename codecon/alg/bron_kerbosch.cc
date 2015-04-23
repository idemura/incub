#include <algorithm>
#include <map>
#include <string>
#include <vector>
#include <utility>
#include <assert.h>
#include <ctype.h>
#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

#define ARRAY_SIZEOF(A) (sizeof(A) / sizeof(A[0]))
#define NON_COPYABLE(C) \
    C(const C&) = default; \
    C& operator=(const C&) = default;

using i64 = long long int;

class Set {
public:
  Set(): bm_() {}

  Set add(int i)
  {
    return Set(bm_ | (1u << i));
  }
  Set rem(int i)
  {
    return Set(bm_ & ~(1u << i));
  }
  Set isect(Set other) const
  {
    return Set(bm_ & other.bm_);
  }
  Set unite(Set other) const
  {
    return Set(bm_ | other.bm_);
  }
  bool in(int i) const
  {
    return (bm_ & (1u << i)) != 0;
  }
  bool empty() const
  {
    return bm_ == 0;
  }
  void print() const
  {
    printf("{ ");
    for (int i = 0; i < 30; i++) {
      if (in(i)) {
        printf("%d ", i + 1);
      }
    }
    printf("}\n");
  }

private:
  explicit Set(unsigned int bm): bm_(bm) {}
  unsigned int bm_;
};

void rprintf(int d, const char *fmt, ...)
{
  char tab[120] = {};
  int j = 0;
  for (int i = 0; i < d; i++) {
    tab[j++] = ' ';
    tab[j++] = ' ';
  }
  printf("%s", tab);
  va_list va;
  va_start(va, fmt);
  vprintf(fmt, va);
  va_end(va);
}

void bronKerboschRec(int d, Set *am, int n, Set r, Set c, Set x)
{
  // rprintf(d, "bronKolchRec:\n");
  // rprintf(d, "c: ");
  // c.print();
  // rprintf(d, "r: ");
  // r.print();
  // rprintf(d, "x: ");
  // x.print();
  if (c.empty()) {
    if (x.empty()) {
      r.print();
    }
    // rprintf(d, "no more candidates\n");
    return;
  }
  // Actually, go through `c`s elements. No pivoting.
  for (int i = 0; i < n; i++) {
    if (!c.in(i)) continue;
    // rprintf(d, "vertex %d\n", i + 1);
    bronKerboschRec(d + 1, am, n, r.add(i), c.isect(am[i]), x.isect(am[i]));
    x = x.add(i);
    c = c.rem(i);
    // rprintf(d, "new x: ");
    // x.print();
    // rprintf(d, "new c: ");
    // c.print();
  }
  // rprintf(d, "end.\n");
}

void bronKerbosch(Set *am, int n)
{
  Set c;
  for (int i = 0; i < n; i++) {
    c = c.add(i);
  }
  bronKerboschRec(0, am, n, Set(), c, Set());
}

void addEdge(Set *am, int a, int b)
{
  a--; b--;  // Zero based.
  am[a] = am[a].add(b);
  am[b] = am[b].add(a);
}

int main(int argc, char **argv)
{
#ifndef ONLINE_JUDGE
  freopen("in", "r", stdin);
#endif
  Set am[6];
  const int n = ARRAY_SIZEOF(am);
  addEdge(am, 1, 2);
  addEdge(am, 1, 5);
  addEdge(am, 2, 3);
  addEdge(am, 2, 5);
  addEdge(am, 3, 4);
  addEdge(am, 4, 5);
  addEdge(am, 4, 6);
  bronKerbosch(am, n);
  return 0;
}
