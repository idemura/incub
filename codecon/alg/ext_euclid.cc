#include <algorithm>
#include <map>
#include <string>
#include <vector>
#include <assert.h>
#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define ARRAY_SIZEOF(A) (sizeof(A) / sizeof(A[0]))
#define INF 0x7fffffff
#define NON_COPYABLE(C) \
    C(const C&); \
    C& operator=(const C&);

typedef long long int lli;

int extEuclid(int m, int n, int* km, int *kn) {
  int x = m, y = n;
  int xu = 1, xv = 0;
  int yu = 0, yv = 1;
  // Maintain the following predicate:
  //  x = xu * m + xv * n;
  //  y = yu * m + yv * n;
  // Each step we have y[n+1] = x[n] - q * y[n], hence formulas.
  int r = x % y;
  while (r) {
    int t;
    int q = x / y;
    x = y;
    y = r;
    t = xu;
    xu = yu;
    yu = t - q * yu;
    t = xv;
    xv = yv;
    yv = t - q * yv;
    r = x % y;
  }
  *km = yu;
  *kn = yv;
  return y;
}

void test(int m, int n, int gcd_expected) {
  int km, kn;
  int gcd = extEuclid(m, n, &km, &kn);
  assert(gcd == gcd_expected);
  assert(m % gcd == 0);
  assert(n % gcd == 0);
  assert(gcd == km * m + kn * n);
}

int main(int argc, char **argv) {
#ifndef ONLINE_JUDGE
  freopen("in", "r", stdin);
#endif
  test(4, 6, 2);
  test(6, 4, 2);
  test(9, 4, 1);
  test(3, 7, 1);
  test(12, 15, 3);
  test(10, 15, 5);
  printf("All tests passed.\n");
  return 0;
}
