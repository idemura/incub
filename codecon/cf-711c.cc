#include <bits/stdc++.h>

using namespace std;

using i64 = long long int;


template<class T>
void read_vector(vector<T> &v) {
  for (auto &x : v) cin>>x;
}

// Map: (beauty, color) => min price.
i64 dp[2][102][102];
int n, m, k;

void dp_reset(int i) {
  memset(dp[i][0][0], -1, sizeof(dp[i]));
}

void dp_update(int i, int b1, int c1, int c2, i64 add_price) {
  auto b2 = c1 == c2? b1: b1 + 1;
  if (b2 > k) return;
  if (dp[1 - h][b2][c2] < 0 || dp[h][b1][c1] + add_price < dp[1 - h][b2][c2]) {
    dp[1 - h][b2][c2] = dp[h][b1][c1] + add_price;
  }
}

pair<int, int> new_bc(pair<int, int> bc, int c) {
  if (bc.second != c) {
    return make_pair(bc.first + 1, c);
  } else {
    return bc;
  }
}

int main() {
  ios::sync_with_stdio (false);
  cin>>n>>m>>k;
  vector<int> t(n);
  read_vector(t);
  vector<vector<int>> p(n);
  for (auto &pi : p) {
    pi.resize(m);
    read_vector(pi);
  }
  dp_reset(0);
  if (t[0] == 0) {
    for (int c = 1; c <= m; c++) {
      dp[0][1][c] = p[0][c - 1];
    }
  } else {
    dp[0][1][t[0]] = 0;
  }
  int h = 0;
  for (int i = 1; i < t.size(); i++) {
    dp_reset(1 - h);
    if (t[i] == 0) {
      for (int b = 1; b <= k; b++) {
        for (int c1 = 1; c1 <= m; c1++) {
          if (dp[h][b][c1] < 0) continue;
          for (int c2 = 1; c2 <= m; c2++) {
            dp_update(h, b, c1, c2, p[i][c2 - 1]);
          }
        }
      }
    } else {
      for (int b = 1; b <= k; b++) {
        for (int c1 = 1; c1 <= m; c1++) {
          if (dp[h][b][c1] < 0) continue;
          for (int c2 = 1; c2 <= m; c2++) {
            dp_update(h, b, c1, c2, p[i][c2 - 1]);
          }
        }
      }
      for (auto &kv : dp) {
        dp_update(new_dp, new_bc(kv.first, t[i]), kv.second);
      }
    }
    dp = move(new_dp);
  }
  i64 min_p = -1;
  for (auto &kv : dp) {
    if (kv.first.first != k) continue;
    if (min_p < 0 || kv.second < min_p) {
      min_p = kv.second;
    }
  }
  cout<<min_p<<endl;
  return 0;
}

