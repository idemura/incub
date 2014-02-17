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
#define INF 0x7fffffff

typedef long long int lli;

struct Node {
  Node *n, *p;
  std::vector<int> xs;

  Node(): n(), p() {}
};

struct Problem {
  Node nmem[10002];
  int nmem_n;
  int x, y;

  Problem(): nmem_n(), x(), y() {}

  void solve()
  {
    char c;
    scanf("%c", &c);  // c == 'P'
    for (scanf("%c", &c); c != 'K'; scanf("%c", &c)) {
      switch (c) {
        case 'S':
          //
          break;

      }
    }
  }
};

int main(int argc, char **argv)
{
// #ifndef ONLINE_JUDGE
//   freopen("in", "r", stdin);
// #endif
  for (int i = 0; i < 1; i++) {
    Problem* p = new Problem();
    p->solve();
    delete p;
  }
  return 0;
}
