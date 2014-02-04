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

int gcd(int a, int b)
{
  // Assumes `b < a`, otherwise the first iteration will swap `a` and `b`.
  while (b != 0) {
    int t = a % b;
    a = b;
    b = t;
  }
  return a;
}

int cs[500000];  // It's too much though.
lli Cnk(int n, int k)
{
  if (n - k < k) {
    k = n - k;
  }
  for (int i = 0; i < k; i++) {
    cs[i] = n - i;
  }
  for (int j = k; j > 1; j--) {
    int dj = n % j, d = j;
    for (; d > 1; dj += j) {
      int g = gcd(cs[dj], d);
      if (g > 1) {
        cs[dj] /= g;
        d /= g;
      }
    }
  }
  lli p = 1;
  for (int i = 0; i < k; i++) {
    p *= cs[i];
  }
  return p;
}

void solve()
{
  // So, we need count sums of k summands such that s1+s2+...sk=n and each
  // si>=1. We can chop 1 from each summand and re-phrase: s1+s2+...sk=m,
  // si>=0, m=n-k.
  // For later we have recursive solution F(k, n):
  // F(1, m) = 1
  // F(k, 0) = 1
  // F(k, m) = SUM i=0..m: F(k-1, m-i)
  // Consider F(k, n) and F(k, m-1):
  //   F(k,m)  =F(k-1,m)+F(k-1,m-1)+F(k-1,m-2)+...+F(k-1,0)
  //   F(k,m-1)=         F(k-1,m-1)+F(k-1,m-2)+...+F(k-1,0)
  // Tails are the same, so we have:
  //   F(k, m) = F(k-1, m) + F(k, m-1)
  // This is almost binomial coefficient! Consider the table, binomial
  // coefficient are on diagonals. So we have:
  //   F(k, m) = C(k-1, m+k-1).
  // Return back to initial notation:
  //   F(k, m) = C(k-1, n-1).
  // So all we have to do is just compute binomial coefficient.
  int n, k;
  scanf("%d%d", &n, &k);
  printf("%lld\n", Cnk(n - 1, k - 1));
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
