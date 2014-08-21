#include <algorithm>
#include <map>
#include <string>
#include <vector>
#include <assert.h>
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

typedef long long int lli;

double a, b, c, d, h0, f0;

void solve() {
  double h = h0, f = f0;
  // Warm-up.
  for (int i = 0; i < 8; i++) {
    double h1 = a * h - b * f;
    double f1 = c * f + d * h;
    h = h1;
    f = f1;
  }

  int hn = 0, hp = 0;
  int fn = 0, fp = 0;
  for (int i = 0; i < 30; i++) {
    double h1 = a * h - b * f;
    double f1 = c * f + d * h;
    if (h1 < h) hn++; else hp++;
    if (f1 < f) fn++; else fp++;
    h = h1;
    f = f1;
    printf("%.10lg %.10lg\n", h, f);
  }
  const int kMargin = 4;
  if (fabs(h) < 0.5 && fabs(f) < 0.5) {
    printf("Ecological balance will develop.\n");
  } else if (hn < kMargin && fn < kMargin) {
    printf("Both hares and foxes will overgrow.\n");
  } else if (hn < kMargin && fp < kMargin) {
    printf("Hares will overgrow while foxes will die out.\n");
  } else if (hp < kMargin && fp < kMargin) {
    printf("Both hares and foxes will die out.\n");
  } else if (hp < kMargin && fn < kMargin) {
    printf("Hares will die out while foxes will overgrow.\n");
  } else {
    printf("Chaos will develop.\n");
  }
}

int main(int argc, char **argv) {
#ifndef ONLINE_JUDGE
  freopen("in", "r", stdin);
#endif
  int n = 0;
  scanf("%d", &n);
  for (int i = 0; i < n; i++) {
    scanf("%lf%lf%lf%lf%lf%lf", &a, &b, &c, &d, &h0, &f0);
    solve();
  }
  return 0;
}