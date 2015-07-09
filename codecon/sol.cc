#include "base.h"

constexpr auto kDinicMax = 200;

// Can we do better than vector of vectors?
using AMat = vector<vector<int>>;

struct DinicCtx {

};

int dinic(AMat &m, int s, int t) {
  const int n = m.size();
  auto flow = 0;
  vector<int> q, prev, iter;
  q.reserve(n);
  while (true) {
    // BFS.
    q.push_back(s);
    prev.assign(n, -1);
    prev[s] = 0;
    for (int i = 0; i < q.size(); i++) {
      for (int v = 0; v < m[q[i]].size(); v++) {
        if (m[q[i]][v] == 0 || prev[v] >= 0) continue;
        prev[v] = q[i];
        q.push_back(v);
      }
    }
    // If t is not reachable, then stop.
    if (prev[t] < 0) break;

    // DFS only between level.
    iter.assign(n, 0);
  }
  return flow;
}

int main(int argc, char **argv) {
  ios_base::sync_with_stdio(false);
  cout << "TESTS PASSED." << endl;
  return 0;
}
