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

struct Node {
  vector<int> adj;
};

// Indices of the tree leaves.
struct TreeLeaves {
  vector<int> idx;
};

constexpr int DIM = 100;

Node nodes[DIM] = {};
int cost[DIM][DIM] = {};

int main(int argc, char **argv) {
  int n = 0, q_limit = 0;
  cin >> n >> q_limit;

  for (int i = 0; i < n; i++) {
    int a, b, c;
    cin >> a >> b >> c;
    a--;
    b--;
    cost[a][b] = cost[b][a] = c;
    nodes[a].push_back(b);
    nodes[b].push_back(a);
  }

  // Perform BFS.
  vector<TreeLeaves>
  for (int q = 0; q < q_limit; q++) {
  }

  return 0;
}
