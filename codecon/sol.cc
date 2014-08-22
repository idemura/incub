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

struct Node {
  // Count of open right parens on the left end, and open left parens on the
  // right end.
  int r, l;
  Node(): r(), l() {}
};
typedef vector<Node> Bit;

Node fromChar(char c) {
  Node pb;
  pb.r = c == ')'? 1: 0;
  pb.l = c == '('? 1: 0;
  return pb;
}

void init(Bit &bit, int leaf_n) {
  int n = leaf_n;
  int s = 0;
  while (true) {
    s += n;
    if (n == 1) break;
    n = (n + 1) / 2;
  }
  bit.resize(s);
}

void print(const Bit &bit, int leaf_n) {
  int j = 0;
  int n = leaf_n;
  while (true) {
    for (int i = 0; i < n; i++) {
      printf("r %d l %d, ", bit[j].r, bit[j].l);
      j++;
    }
    printf("\n");
    if (n == 1) break;
    n = (n + 1) / 2;
  }
}

Node sum(const Node &lh, const Node &rh) {
  Node r;
  int m = std::min(lh.l, rh.r);
  r.l = lh.l + rh.l - m;
  r.r = lh.r + rh.r - m;
  return r;
}

void create(Bit &bit, const string &str) {
  int leaf_n = str.size();
  init(bit, leaf_n);
  for (int i = 0; i < leaf_n; i++) {
    Node pb = fromChar(str[i]);
    bit[i] = pb;
    int n = leaf_n;
    int s = n;
    int j = i;
    while (n > 1) {
      bit[s + j / 2] = sum(bit[s + j / 2], pb);
      n = (n + 1) / 2;
      s += n;
      j /= 2;
    }
  }
}

bool check(const Bit &bit) {
  Node pb = bit[bit.size() - 1];
  return pb.r == 0 && pb.l == 0;
}

void update(Bit &bit, string &str, int i) {
  int leaf_n = str.size();
  str[i] = str[i] == '('? ')': '(';
  bit[i] = fromChar(str[i]);
  int n = leaf_n;
  int s = n;
  int j = i;
  while (n > 1) {
    if (j & 1) {
      bit[s + j / 2] = sum(bit[j - 1], bit[j]);
    } else {
      bit[s + j / 2] = sum(bit[j], j + 1 < n? bit[j + 1]: Node());
    }
    j /= 2;
    n = (n + 1) / 2;
    s += n;
  }
}

int main(int argc, char **argv) {
#ifndef ONLINE_JUDGE
  freopen("in", "r", stdin);
#endif
  string str;
  Bit bit;
  for (int i = 0; i < 10; i++) {
    printf("Test %d:\n", i + 1);
    int n = 0;
    scanf("%d", &n);
    str.resize(n);
    scanf("%s", &str[0]);

    create(bit, str);
    int m = 0;
    scanf("%d", &m);
    for (int j = 0; j < m; j++) {
      int op = 0;
      scanf("%d", &op);
      if (op == 0) {
        bool valid = check(bit);
        printf("%s\n", valid? "YES": "NO");
      } else {
        update(bit, str, op - 1);
      }
    }
  }
  return 0;
}
