#include <algorithm>
#include <map>
#include <string>
#include <vector>
#include <stdio.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define INF 0x7fffffff

using namespace std;

typedef long long int lli;

int x, y, k, b, ps_n, c;
lli ps[40];

void comb(int i, int k1, lli sum)
{
  if (sum > y) {
    return;
  }
  if (k1 == 0) {
    if (x <= sum && sum <= y) {
      c++;
    }
    return;
  }
  for (int j = i; j + k1 <= ps_n; j++) {
    comb(j + 1, k1 - 1, sum + ps[j]);
  }
}

int main()
{
#ifndef ONLINE_JUDGE
  freopen("in", "r", stdin);
#endif
  int i;
  lli p;
  scanf("%d%d%d%d", &x, &y, &k, &b);
  for (i = 0, p = 1; p <= y; i++, p *= b) {
    ps[i] = p;
  }
  ps_n = i;
  comb(0, k, 0);
  printf("%d\n", c);
  return 0;
}
