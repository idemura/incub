#include <algorithm>
#include <functional>
#include <iostream>
#include <map>
#include <string>
#include <queue>
#include <vector>
#include <assert.h>
#include <ctype.h>
#include <limits.h>
#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define ARRAY_SIZEOF(A) (sizeof(A) / sizeof(A[0]))
#define INF 0x7fffffff
#define NON_COPYABLE(C) \
    C(const C&); \
    C& operator=(const C&);

using namespace std;

typedef long long int i64;
typedef __int128 i128;

struct M2x2i {
  i64 m11 = 0, m12 = 0, m21 = 0, m22 = 0;
};

i128 mul_i128(i64 a, i64 b) {
  return i128(a) * i128(b);
}

M2x2i mulMatrix(const M2x2i &a, const M2x2i &b, i64 m) {
  M2x2i r;
  r.m11 = (mul_i128(a.m11, b.m11) + mul_i128(a.m12, b.m21)) % m;
  r.m12 = (mul_i128(a.m11, b.m12) + mul_i128(a.m12, b.m22)) % m;
  r.m21 = (mul_i128(a.m21, b.m11) + mul_i128(a.m22, b.m21)) % m;
  r.m22 = (mul_i128(a.m21, b.m12) + mul_i128(a.m22, b.m22)) % m;
  return r;
}

M2x2i powMatrix(const M2x2i &a, i64 p, i64 m) {
  M2x2i f = a, r;
  r.m11 = r.m22 = 1;
  for (i64 k = m; k != 0; k >>= 1) {
    if (k & 1) {
      r = mulMatrix(f, r, m);
    }
    f = mulMatrix(f, f, m);
  }
  return r;
}

int main(int argc, char **argv) {
  i64 n = 0, k = 0, m = 0;
  cin >> n >> k >> m;
  M2x2i r;
  r.m11 = 0;
  r.m12 = k - 1;
  r.m21 = k - 1;
  r.m22 = k - 1;
  M2x2i r_to_n = powMatrix(r, n - 1, m);
  cout << (r_to_n.m12 * (k - 1)) << endl;
  return 0;
}
