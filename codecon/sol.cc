#include <algorithm>
#include <functional>
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

typedef long long int lli;
typedef vector<int> VectorInt;

struct DfsState {
  vector<VectorInt> *m;
  int counter;
  VectorInt mark;
  vector<VectorInt> vert_id_to_walk_id;
  VectorInt walk_id_to_vert_id;
  VectorInt walk;

  DfsState(int n, vector<VectorInt> *m):
      m(m), counter(), mark(n), vert_id_to_walk_id(n) {
  }

  int assignWalkId(int v) {
    int walk_id = counter++;
    assert(walk_id == walk_id_to_vert_id.size());
    vert_id_to_walk_id[v].push_back(walk_id);
    walk_id_to_vert_id.push_back(v);
    return walk_id;
  }
};

void dfs(DfsState &state, int v) {
  printf("dfs vertex %d\n", v + 1);
  const VectorInt &a = (*state.m)[v];
  int w_id = state.assignWalkId(v);
  printf("  %d: assigned walk id %d\n", v + 1, w_id);

  if (state.mark[v] || a.size() == 0) {
    state.walk.push_back(w_id);
    printf("  %d: added to walk %d, imm return\n", v + 1, w_id);
    return;
  }

  state.mark[v] = 1;
  for (int i = 0; i < a.size(); i++) {
    dfs(state, a[i]);
    printf("%d: added to walk %d\n", v + 1, w_id);
    state.walk.push_back(w_id);
  }
  printf("  %d: return\n", v + 1);
}

int getRmqSize(int n) {
  int len = 1;
  while (len < n) {
    len *= 2;
  }
  return len;
}

int rmq(const vector<VectorInt> &rmq, int a, int b) {
  return -1;
}

int bulkRmq(const VectorInt& vs, const vector<VectorInt> &rmq_data) {
  int mca = vs[0];
  for (int i = 1; i < vs.size(); i++) {
    if (mca == 0) break;
    mca = rmq(rmq_data, mca, vs[i]);
  }
  return mca;
}

void readAndSolve() {
  int n, num_edge;
  scanf("%d%d", &n, &num_edge);
  vector<VectorInt> m(n);
  for (int i = 0; i < num_edge; i++) {
    int s, d;
    scanf("%d%d", &s, &d);
    m[s - 1].push_back(d - 1);
  }
  DfsState state(n, &m);
  dfs(state, 0);

  vector<VectorInt> vert_id_to_walk_id;
  VectorInt walk_id_to_vert_id, walk;
  vert_id_to_walk_id.swap(state.vert_id_to_walk_id);
  walk_id_to_vert_id.swap(state.walk_id_to_vert_id);
  walk.swap(state.walk);

  printf("vert_id_to_walk_id:\n");
  for (int i = 0; i < vert_id_to_walk_id.size(); i++) {
    printf("%d ", i + 1);
    for (int j = 0; j < vert_id_to_walk_id[i].size(); j++) {
      printf("%d ", vert_id_to_walk_id[i][j]);
    }
    printf("\n");
  }
  printf("walk_id_to_vert_id\n");
  for (int i = 0; i < walk_id_to_vert_id.size(); i++) {
    printf("%d -> %d\n", i, walk_id_to_vert_id[i] + 1);
  }
  printf("walk:\n");
  for (int i = 0; i < walk.size(); i++) {
    printf("%d ", walk[i]);
  }
  printf("\n");

  printf("!!!!!!\n");
  // Now we solve RMQ.
  int walk_size = walk.size();
  vector<VectorInt> rmq(getRmqSize(walk_size));
  rmq[0].swap(walk);
  for (int len = 2, i = 1; len <= walk_size; len *= 2, i++) {
    printf("len %d\n", len);
    for (int j = 0; j < rmq[i - 1].size(); j++) {
      printf("%d ", rmq[i - 1][j]);
    }
    printf("\n");
    VectorInt &v = rmq[i];
    v.resize(walk_size + 1 - len);
    for (int j = 0; j < v.size(); j++) {
      assert(j + len / 2 < rmq[i - 1].size());
      printf("  at %d min of %d %d\n", j, rmq[i - 1][j], rmq[i - 1][j + len / 2]);
      v[j] = std::min(rmq[i - 1][j], rmq[i - 1][j + len / 2]);
    }
  }

  VectorInt critical;
  critical.push_back(0);
  for (int i = 1; i < vert_id_to_walk_id.size(); i++) {
    const VectorInt &walk_ids = vert_id_to_walk_id[i];
    if (walk_ids.size() == 1) {
      if (m[i].size() == 0) {  // ... and not a leaf.
        critical.push_back(i);
      }
    } else {
      int mca = bulkRmq(walk_ids, rmq);
      if (mca != 0) {
        critical.push_back(walk_id_to_vert_id[mca]);
      }
    }
  }

  sort(critical.begin(), critical.end());
  printf("%zd\n", critical.size());
  for (int i = 0; i < critical.size(); i++) {
    printf("%d ", critical[i] + 1);
  }
  printf("\n");
}

int main(int argc, char **argv) {
#ifndef ONLINE_JUDGE
  freopen("in", "r", stdin);
  int tests = 1;
#else
  int tests = 10;
#endif
  for (int i = 0; i < tests; i++) {
    readAndSolve();
  }
  return 0;
}

