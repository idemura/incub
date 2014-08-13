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

struct Pair {
  double h, f;

  Pair(): h(), f() {}
  Pair(double h, double f): h(h), f(f) {}
};

double a, b, c, d;
Pair p0;

Pair step(Pair p) {
  return Pair(a * p.h - b * p.f,
              c * p.f + d * p.h);
}

bool softZero(double x) {
  return fabs(x) < 1E-9;
}

bool magnitude(Pair *p, double *m) {
  double t = std::max(fabs(p->h), fabs(p->f));
  if (softZero(t)) {
    return false;
  }
  p->h /= t;
  p->f /= t;
  *m += log(t);
  return true;
}

bool quadraticEq(double *x0, double *x1) {
  double a_c = a - c;
  double D = a_c * a_c - 4 * b * d;
  if (D < 0) {
    return false;
  }
  double sqrt_D = sqrt(D);
  *x0 = (a_c - sqrt_D) / (2 * d);
  *x1 = (a_c + sqrt_D) / (2 * d);
  return true;
}

int main(int argc, char **argv) {
#ifndef ONLINE_JUDGE
  freopen("in", "r", stdin);
#endif
  int n = 0;
  scanf("%d", &n);
  const int kMaxStep = 120;
  for (int i = 0; i < n; i++) {
    scanf("%lf%lf%lf%lf%lf%lf", &a, &b, &c, &d, &p0.h, &p0.f);
    double x0, x1;
    if (!quadraticEq(&x0, &x1)) {
      printf("Chaos will develop.\n");
      continue;
    }

    bool balance = false;
    Pair p = p0;
    double m = 0;
    magnitude(&p, &m);
    for (int i = 0; i < kMaxStep; i++) {
      // printf("%.10lg %.10lg m = %.10lg\n", p.h, p.f, m);
      p = step(p);
      double m_prev = m;
      if (!magnitude(&p, &m)) {
        balance = true;
        break;
      }
      if (fabs(m / m_prev - 1.0) < 1E-9) {
        break;
      }
    }
    if (balance) {
      printf("Ecological balance will develop.\n");
    } else {
      if (p.h > 0 && p.f > 0) {
        printf("Both hares and foxes will overgrow.\n");
      } else if (p.h > 0 && p.f < 0) {
        printf("Hares will overgrow while foxes will die out.\n");
      } else if (p.h < 0 && p.f < 0) {
        printf("Both hares and foxes will die out.\n");
      } else {
        printf("Hares will die out while foxes will overgrow.\n");
      }
    }
  }
  return 0;
}
