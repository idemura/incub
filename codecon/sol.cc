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
  int open_r = 0, open_l = 0;
  Node *p = nullptr;
  Node *l = nullptr;
  Node *r = nullptr;
};

struct Tree {
  vector<Node> nodes;
  Node *root = 0;
};

void buildTreeStep(const string& str, int a, int b, Tree* tree) {

}

void buildTree(const string& str, Tree* tree) {

}

int main(int argc, char **argv) {
#ifndef ONLINE_JUDGE
  freopen("in", "r", stdin);
#endif
  string s;
  for (int i = 0; i < 1; i++) {
    int n = 0;
    scanf("%d", &n);
    s.resize(n);
    scanf("%s", s.data());
    int m = 0;

    // Heap-like node indexing.
    Tree tree;
    buildTree();

    scanf("%s", &m);
    for (int j = 0; j < m; j++) {
    }
  }
  return 0;
}
