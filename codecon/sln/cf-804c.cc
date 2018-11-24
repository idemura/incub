#include <algorithm>
#include <cassert>
#include <cstdio>
#include <iostream>
#include <unordered_map>
#include <vector>

using namespace std;

using i64 = long long int;

int n, m;
int color_max;
int mark;

vector<vector<int>> ice, adj;
vector<int> color,
        no_color, // Ice with no color
        vc; // Ice colors, temporary to find minimal unused color in a vertex.

void dfs(int v, int prev) {
    mark++;

    // Find minimal unused color to color a vertex
    no_color.clear();
    for (auto u : ice[v]) {
        if (color[u] == 0) {
            no_color.push_back(u);
        } else {
            vc[color[u]] = mark;
        }
    }
    int c = 1;
    for (int i = 0; i < no_color.size(); i++) {
        while (vc[c] == mark)
            c++;
        color[no_color[i]] = c;
        if (c > color_max) {
            color_max = c;
        }
        c++; // Search from next
    }

    for (auto u : adj[v]) {
        if (u == prev) continue;
        dfs(u, v);
    }
}

int main() {
    scanf("%d%d", &n, &m);
    adj.resize(n);
    ice.resize(n);
    for (int i = 0; i < n; i++) {
        int q;
        scanf("%d", &q);
        ice[i].resize(q);
        for (auto &x : ice[i]) {
            scanf("%d", &x);
        }
    }
    for (int i = 1; i < n; i++) {
        int a, b;
        scanf("%d%d", &a, &b);
        adj[a - 1].push_back(b - 1);
        adj[b - 1].push_back(a - 1);
    }

    color.resize(m + 1);
    vc.resize(m + 1);
    dfs(0, -1);

    for (int i = 1; i < color.size(); i++) {
        if (color[i] == 0) {
            color[i] = 1;
            if (color[i] > color_max) {
                color_max = color[i];
            }
        }
    }

    printf("%d\n", color_max);
    for (int i = 1; i < color.size(); i++) {
        printf("%d ", color[i] ? color[i] : 1);
    }
    printf("\n");

    return 0;
}
