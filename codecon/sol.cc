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
  // Count of open right parens on the left end, and open left parens on the
  // right end.
  int r, l;
  ParenBal(): r(), l() {}
};
typedef vector<ParenBal> ParenBalVector;

ParenBal fromChar(char c) {
  ParenBal pb;
  pb.r = c == ')'? 1: 0;
  pb.l = c == '('? 1: 0;
  return pb;
}

struct Bit {
  int n;
  vector<ParenBal> t;

  Bit(): n() {}
  void init(int str_len) {
    int n = str_len;
    int s = 0;
    while (true) {
      s += n;
      if (n == 1) break;
      n = (n + 1) / 2;
    }
    t.resize(s);
    this->n = str_len;
  }

  void print() const {
    int j = 0;
    int n = this->n;
    while (true) {
      for (int i = 0; i < n; i++) {
        printf("r %d l %d, ", t[j].r, t[j].l);
        j++;
      }
      printf("\n");
      if (n == 1) break;
      n = (n + 1) / 2;
    }
  }
};

ParenBal sum(const ParenBal &lh, const ParenBal &rh) {
  ParenBal r;
  int m = std::min(lh.l, rh.r);
  r.l = lh.l + rh.l - m;
  r.r = lh.r + rh.r - m;
  return r;
}

// int countStepsToSum(int n) {
//   assert(n > 0);
//   int c = 0;
//   while (!(n & 1)) {
//     c++;
//     n >>= 1;
//   }
//   return c;
// }

void create(const vector<char> &str, Bit &bit) {
  printf("string %s\n", &str[0]);
  bit.init(str.size());
  for (int i = 0; i < str.size(); i++) {
    ParenBal pb = fromChar(str[i]);
    bit.t[i] = pb;
    int n = bit.n;
    int s = n;
    int j = i;
    printf("----\n");
    while (n > 1) {
      printf("update index %d\n", s + j / 2);
      bit.t[s + j / 2] = sum(bit.t[s + j / 2], pb);
      n = (n + 1) / 2;
      s += n;
      j /= 2;
    }
  }
  bit.print();
}

bool check(const Bit &bit) {
  ParenBal pb = bit.t[bit.t.size() - 1];
  return pb.r == 0 && pb.l == 0;
}

void update(int i, vector<char> &str, Bit &bit) {
  str[i] = str[i] == '('? ')': '(';
  printf("new string %s\n", &str[0]);
  bit.t[i] = fromChar(str[i]);
  int n = bit.n;
  int s = n;
  int j = i;
  while (n > 1) {
    if (j & 1) {
      bit.t[s + j / 2] = sum(bit.t[j - 1], bit.t[j]);
    } else {
      bit.t[s + j / 2] = sum(bit.t[j], j + 1 < n? bit.t[j + 1]: ParenBal());
    }
    j /= 2;
    n = (n + 1) / 2;
    s += n;
  }
  bit.print();
}

int main(int argc, char **argv) {
#ifndef ONLINE_JUDGE
  freopen("in", "r", stdin);
#endif
  vector<char> s;
  Bit bit;
  for (int i = 0; i < 1; i++) {
    int n = 0;
    scanf("%d", &n);
    s.resize(n);
    scanf("%s", &s[0]);

    create(s, bit);

    int m = 0;
    scanf("%d", &m);
    for (int j = 0; j < m; j++) {
      int op = 0;
      scanf("%d", &op);
      if (op == 0) {
        bool valid = check(bit);
        printf("%s\n", valid? "YES": "NO");
      } else {
        update(op - 1, s, bit);
      }
    }
  }
  return 0;
}
