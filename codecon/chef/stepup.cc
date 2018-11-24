#include <algorithm>
#include <functional>
#include <iostream>
#include <list>
#include <map>
#include <math.h>
#include <random>
#include <sstream>
#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#define ARRAY_SIZEOF(A) (sizeof(A) / sizeof(A[0]))

using namespace std;

using i64 = long long int;
using i32 = int;

constexpr char kEol[] = "\n";
constexpr int INF = 0x7fffffff;
constexpr int MOD = 1000000009;

int dfs(const vector<vector<int>> &a, vector<int> &mark, int v, int len) {
    if (mark[v]) return -1;
    mark[v] = 1;
    int max_len = len;
    for (auto w : a[v]) {
        int w_len = dfs(a, mark, w, len + 1);
        if (w_len < 0) return -1;
        max_len = max(max_len, w_len);
    }
    return max_len;
}

int main(int argc, char **argv) {
    ios_base::sync_with_stdio(false);
    int t = 0;
    cin >> t;
    while (t--) {
        int n, m;
        cin >> n >> m;
        vector<vector<int>> a(n);
        vector<int> in_degree(n);
        for (int i = 0; i < m; i++) {
            int x, y;
            cin >> x >> y;
            x--;
            y--;
            a[x].push_back(y);
            in_degree[y]++;
        }
        int max_len = -1;
        for (int i = 0; i < n; i++) {
            if (in_degree[i]) continue;
            vector<int> mark(n);
            int len = dfs(a, mark, i, 1);
            if (len < 0) {
                max_len = -1;
                break;
            }
            max_len = max(max_len, len);
        }
        if (max_len < 0) {
            cout << "IMPOSSIBLE\n";
        } else {
            cout << max_len << endl;
        }
    }
    return 0;
}
