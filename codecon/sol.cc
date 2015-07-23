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

struct PairWithLen {
  pair<int, int> p;
  int d = 0;
  PairWithLen() {}
  PairWithLen(pair<int, int> p, int d): p(p), d(d) {}
};

struct Context {
  int n;
  int a[DIM + 1][DIM] = {};
  int dist_to_exit[DIM + 1] = {};
  vector<int> dp;

  explicit Context(int n) : n(n), dp((1<<DIM) + 4) {}
};

void compute_dist(
    pair<int, int> start,
    const vector<pair<int, int>> &pos,
    const vector<string> &m,
    int *row,
    int *dist_to_exit) {
  const auto n = m.size();
  vector<PairWithLen> q{{start, 0}};
  int f[DIM][DIM] = {};
  for (int i = 0; i < q.size(); i++) {
    int x = q[i].p.first, y = q[i].p.second;
    if (f[x][y] || m[x][y] == '#') continue;
    f[x][y] = 1;
    if (m[x][y] == '*') {
      row[find(pos.begin(), pos.end(), q[i].p) - pos.begin()] = q[i].d;
    } else if (x == n - 1 && y == n - 1) {
      *dist_to_exit = q[i].d;
    }
    if (x - 1 >= 0 && !f[x - 1][y]) {
      q.emplace_back(make_pair(x - 1, y), q[i].d + 1);
    }
    if (y - 1 >= 0 && !f[x][y - 1]) {
      q.emplace_back(make_pair(x, y - 1), q[i].d + 1);
    }
    if (x + 1 < n && !f[x + 1][y]) {
      q.emplace_back(make_pair(x + 1, y), q[i].d + 1);
    }
    if (y + 1 < n && !f[x][y + 1]) {
      q.emplace_back(make_pair(x, y + 1), q[i].d + 1);
    }
  }
}

void print_row(pair<int, int> start, int *row, int dist_to_exit,
    const vector<pair<int, int>> &pos) {
  cout<<"From #"<<start.first<<","<<start.second<<":"<<endl;
  for (int i = 0; i < pos.size(); i++) {
    cout<<"  to "<<pos[i].first<<","<<pos[i].second<<": "<<row[i]<<endl;
  }
  cout<<"  to exit: "<<dist_to_exit<<endl;
}

int dp_step(int v, int mask, Context &c) {
  auto &dp_i = c.dp[(v << 13) | mask];
  if (dp_i < 0) {
    if (mask == (1<<c.n) - 1) {
      dp_i = c.dist_to_exit[v];
    } else {
      int min_d = INF;
      for (int i = 0; i < c.n; i++) {
        if (mask & (1<<i) || c.a[v][i] == 0) continue;
        min_d = min(min_d, dp_step(i, mask | (1<<i), c) + c.a[v][i]);
      }
    }
  }
  return dp_i;
}

int read_solve() {
  int n;
  cin >> n;
  vector<pair<int, int>> pos;
  vector<string> m(n);
  for (int i = 0; i < n; i++) {
    cin >> m[i];
    for (int j = 0; j < n; j++) {
      if (m[i][j] == '*') {
        pos.emplace_back(i, j);
        cout<<"vertex "<<pos.size()<<": "<<i<<","<<j<<endl;
      }
    }
  }
  Context c(m.size());
  compute_dist(make_pair(0, 0), pos, m, c.a[0], &c.dist_to_exit[0]);
  print_row(make_pair(0, 0), c.a[0], c.dist_to_exit[0], pos);
  for (int i = 0; i < pos.size(); i++) {
    compute_dist(pos[i], pos, m, c.a[i + 1], &c.dist_to_exit[i + 1]);
    print_row(pos[i], c.a[i + 1], c.dist_to_exit[i + 1], pos);
  }
  return dp_step(0, 0, c);
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
