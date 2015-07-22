#include <algorithm>
#include <functional>
#include <list>
#include <map>
#include <unordered_map>
#include <vector>
#include <string>
#include <random>
#include <sstream>
#include <utility>
#include <iostream>
#include <stdlib.h>
#include <math.h>

using namespace std;

using i64 = long long int;
using i32 = int;

constexpr int INF = 0x7fffffff;
constexpr int MOD = 100000007;
constexpr int DIM = 13;

using PairOfInts = pair<int, int>;

struct PairWithLen {
  PairOfInts p;
  int d = 0;
  PairWithLen() {}
  PairWithLen(int x, int y, int d): x(x), y(y), d(d) {}
};

void compute_dist(int v,
    const vector<PairOfInts> &pos,
    const vector<string> &m,
    int *row) {
  vector<PairWithLen> q{{pos[i].first, pos[i].second, 0}};
  int f[DIM][DIM] = {};
  for (int i = 0; i < q.size(); i++) {
    int x = q[i].p.first, y = q[i].p.second;
    if (f[x][y]) continue;
    f[x][y] = 1;
    if (m[x][y] == '*') {
      row[pos.find(q[i].p) - pos.begin()] = q[i].len;
    }
    if (x - 1 >= 0 && !f[x - 1][y]) {
      q.emplace_back(x - 1, y, q[i].d + 1);
    }
    if (y - 1 >= 0 && !f[x][y - 1]) {
      q.emplace_back(x, y - 1, q[i].d + 1);
    }
    if (x + 1 < m.size() && !f[x + 1][y]) {
      q.emplace_back(x + 1, y, q[i].d + 1);
    }
    if (y + 1 < m.size() && !f[x][y + 1]) {
      q.emplace_back(x, y + 1, q[i].d + 1);
    }
  }
}

int read_solve(const vector<string> &m) {
  int n;
  cin >> n;
  vector<pair<int, int>> tr_pos{make_pair(0, 0)}, q;
  vector<string> m(n);
  for (int i = 0; i < n; i++) {
    cin >> m[i];
    for (int j = 0; j < n; j++) {
      if (m[i][j] == '*') tr_pos.emplace_back(i, j);
    }
  }
  int a[DIM][DIM] = {};
  for (int i = 0; i < tr_pos.size(); i++) {
    compute_dist(i, tr_pos, m, a[i]);
  }
}

int main(int argc, char **argv) {
  ios_base::sync_with_stdio(false);
  int t;
  cin >> t;
  for (; t > 0; t--) {
    cout << read_solve() << endl;
  }
  return 0;
}
