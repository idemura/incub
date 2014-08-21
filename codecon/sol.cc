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

using namespace std;

typedef long long int lli;

struct ParenBal {
  int open_r, open_l;
  ParenBal(): open_r(), open_l() {}
};

ParenBal sum(const ParenBal &lh, const ParenBal &rh) {
  ParenBal r;
  int m = std::min(lh.open_l, rh.open_r);
  r.open_l = lh.open_l + rh.open_l - m;
  r.open_r = lh.open_r + rh.open_r - m;
  return r;
}

int countStepsToSum(int n) {
  assert(n > 0);
  int c = 0;
  for (; (n & 1) == 0; n >>= 1) {
    c++;
  }
  return c;
}

void buildBIT(const vector<char> &s, vector<ParenBal> &bit) {
  printf("string %s\n", &s[0]);
  bit.resize(s.size());
  for (int i = 0; i < s.size(); i++) {
    ParenBal pb;
    pb.open_r = s[i] == ')'? 1: 0;
    pb.open_l = s[i] == '('? 1: 0;
    int steps_to_sum = countStepsToSum(i + 1);
    for (int s = 0; s < steps_to_sum; s++) {
      int step_i = i - (1 << s);
      pb = sum(bit[step_i], pb);
    }
    bit[i] = pb;
  }

  for (int i = 0; i < bit.size(); i++) {
    printf("bit[%d] r %d l %d\n", i, bit[i].open_r, bit[i].open_l);
  }
}

int main(int argc, char **argv) {
#ifndef ONLINE_JUDGE
  freopen("in", "r", stdin);
#endif
  vector<char> s;
  vector<ParenBal> bit;
  for (int i = 0; i < 1; i++) {
    int n = 0;
    scanf("%d", &n);
    s.resize(n);
    scanf("%s", &s[0]);

    buildBIT(s, bit);

    int m = 0;
    scanf("%d", &m);
    for (int j = 0; j < m; j++) {
      int op = 0;
      scanf("%d", &op);
      if (op == 0) {
      } else {
      }
    }
  }
  return 0;
}
