#include "base.h"

// Can we do better than vector of vectors?
using AMat = vector<vector<int>>;

void print_edges(const AMat &m) {
    cout << "Edges:" << endl;
    const int n = m.size();
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            if (m[i][j] != 0) {
                cout << "  " << i << " - " << j << " cap " << m[i][j] << endl;
            }
        }
    }
}

int update_path(AMat &m, const vector<int> &path) {
    auto cap = numeric_limits<int>::max();
    for (int i = 1; i < path.size(); i++) {
        cap = min(cap, m[path[i - 1]][path[i]]);
    }
    for (int i = 1; i < path.size(); i++) {
        m[path[i - 1]][path[i]] -= cap;
        m[path[i]][path[i - 1]] += cap;
    }
    return cap;
}

int dinic(AMat &m, int s, int t) {
    const int n = m.size();
    auto flow = 0;
    // Resize here to `n`, but actually we just need to reserve. In the cycle we
    // re-assign to proper size any way.
    vector<int> q(n), level, index, path(n);
    while (true) {
        // print_edges(m);
        // BFS, layered network.
        q.assign({s});
        level.assign(n, -1);
        level[s] = 0;
        for (int i = 0; i < q.size() && q[i] != t; i++) {
            for (int v = 0; v < n; v++) {
                if (m[q[i]][v] > 0 && level[v] < 0) {
                    level[v] = level[q[i]] + 1;
                    q.push_back(v);
                }
            }
        }
        // If `t` is not reachable, then stop.
        if (level[t] < 0) break;

        // Finding blocking flow. DFS only between level while it reaches t.
        // Note counters in `index` aren't reset.
        index.assign(n, 0);
        while (true) {
            auto v = s;
            path.assign({v});
            while (!path.empty() && v != t) {
                if (index[v] == n) {
                    path.pop_back();
                    v = path.back();
                    index[v]++;
                } else {
                    auto u = index[v];
                    if (m[v][u] == 0 || level[u] != level[v] + 1) {
                        index[v]++;
                    } else {
                        path.push_back(v = u);
                    }
                }
            }
            if (path.empty()) break;
            flow += update_path(m, path);
        }
    }
    return flow;
}

AMat make_mat(int n) {
    AMat m(n);
    for (auto &v : m)
        v.resize(n);
    return m;
}

void set_edge(AMat &m, int v, int u, int cap) {
    m[v][u] = cap;
}

void test1() {
    AMat m = make_mat(4);
    set_edge(m, 0, 1, 3);
    set_edge(m, 1, 3, 3);
    set_edge(m, 0, 2, 2);
    set_edge(m, 2, 3, 2);
    set_edge(m, 2, 1, 1);
    CHECK(dinic(m, 0, 3) == 5);
}

void test2() {
    AMat m = make_mat(4);
    set_edge(m, 0, 1, 3);
    set_edge(m, 1, 3, 1);
    set_edge(m, 0, 2, 1);
    set_edge(m, 2, 3, 2);
    set_edge(m, 2, 1, 1);
    CHECK(dinic(m, 0, 3) == 2);
}

void test3() {
    AMat m = make_mat(4);
    set_edge(m, 0, 1, 1);
    set_edge(m, 1, 3, 5);
    set_edge(m, 0, 2, 5);
    set_edge(m, 2, 3, 1);
    set_edge(m, 2, 1, 5);
    CHECK(dinic(m, 0, 3) == 6);
}

// This is sample from Wikipedia's page about Dinic algorithm.
void test4() {
    AMat m = make_mat(6);
    set_edge(m, 0, 1, 10);
    set_edge(m, 0, 2, 10);
    set_edge(m, 1, 2, 2);
    set_edge(m, 1, 3, 4);
    set_edge(m, 1, 4, 8);
    set_edge(m, 2, 4, 9);
    set_edge(m, 4, 3, 6);
    set_edge(m, 3, 5, 10);
    set_edge(m, 4, 5, 10);
    CHECK(dinic(m, 0, 5) == 19);
}

int main(int argc, char **argv) {
    ios_base::sync_with_stdio(false);
    test1();
    test2();
    test3();
    test4();
    cout << "TESTS PASSED." << endl;
    return 0;
}
