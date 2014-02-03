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

// Values of sequence we know.
int vs[120];
int vs_n, extra_n;

// Use Newton form method.
int gcd(int a, int b)
{
  if (a < 0) a = -a;
  if (b < 0) b = -b;
  while (b != 0) {
    int t = a % b;
    a = b;
    b = t;
  }
  return a;
}

int mulDiv(int x, int m1, int d1, int m2, int d2)
{
  return (int)((lli)x * m1 * m2 / d1 / d2);
}

int extrapolate(int x)
{
  const int kMax = 20000000;
  int s = 0, n = 1, d = 1;
  // Skip first because it will zero `k`.
  for (int i = 1; i < vs_n; i++) {
    n *= x - i - 1;
    d *= -i;
    if (n > kMax) {
      int g = gcd(n, d);
      n /= g;
      d /= g;
    }
  }
  int k = n / d;
  // Now we can do incrementally: add summand, modify coefficient.
  for (int i = 0; ; i++) {
    s += k * vs[i];
    // We know that `md` is a divisor of `k`, so can divide and than multiply
    // without concerns about overflow or remainders.
    int xi = x - i - 1;
    int ii = i + 1;
    if (ii == vs_n) break;
    k = mulDiv(k, xi, xi - 1, ii - vs_n, ii);
  }
  return s;
}

void solve()
{
  scanf("%d%d", &vs_n, &extra_n);
  for (int i = 0; i < vs_n; i++) {
    scanf("%d", &vs[i]);
  }
  for (int i = 1; i <= extra_n; i++) {
    printf("%d ", extrapolate(i + vs_n));
  }
  printf("\n");
}

int main(int argc, char **argv)
{
#ifndef ONLINE_JUDGE
  freopen("in", "r", stdin);
#endif
  int t = 0;
  scanf("%d", &t);
  for (; t > 0; --t) {
    solve();
  }
  return 0;
}
