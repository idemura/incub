#include <bits/stdc++.h>

using namespace std;

using AdjList = vector<vector<int>>;

bool bipart_dfs(const AdjList &al, vector<int> &vis, int v, int side) {
    if (!vis[v]) {
        vis[v] = 1 + side;
        for (auto a : al[v]) {
            if (!bipart_dfs(al, vis, a, 1 - side)) {
                return false;
            }
        }
        return true;
    } else {
        return 1 + side == vis[v];
    }
}

bool is_bipart(const vector<vector<int>> &al) {
    // 0 - for not visisted,
    // 1 - visited and part 1,
    // 2 - visited and part 2,
    // Although @side is either 0 or 1, which we +1 when write into @vis.
    vector<int> vis(al.size());
    for (int i = 0; i < al.size(); i++) {
        if (vis[i]) continue;
        if (!bipart_dfs(al, vis, i, 0)) {
            return false;
        }
    }
    return true;
}

int main() {
    {
        vector<vector<int>> al{
                {1, 2},
                {0},
                {0},
        };
        assert(is_bipart(al));
    }
    {
        vector<vector<int>> al{
                {1},
                {2},
                {0},
        };
        assert(!is_bipart(al));
    }
    {
        vector<vector<int>> al{
                {1},
                {2},
                {3},
                {0},
        };
        assert(is_bipart(al));
    }
    {
        vector<vector<int>> al{
                {1, 2},
                {2},
                {3},
                {0},
        };
        assert(!is_bipart(al));
    }
    cout << "TESTS PASSED." << endl;
    return 0;
}
