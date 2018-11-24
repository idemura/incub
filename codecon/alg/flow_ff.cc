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

int ford_fulkerson(AMat &m, int s, int t) {
    const int n = m.size();
    auto flow = 0;
    // Resize here to `n`, but actually we just need to reserve. In the cycle we
    // re-assign to proper size any way.
    vector<int> q(n), prev;
    while (true) {
        // BFS, augmenting path.
        prev.assign(n, -1);
        q.assign({s});
        for (int i = 0; i < q.size() && q[i] != t; i++) {
            for (int v = 0; v < n; v++) {
                if (m[q[i]][v] > 0 && prev[v] < 0 && v != s) {
                    prev[v] = q[i];
                    q.push_back(v);
                }
            }
        }
        // If `t` is not reachable, then stop.
        if (prev[t] < 0) break;

        // Update flow along the augmenting path.
        auto cap = numeric_limits<int>::max();
        for (int v = t; prev[v] >= 0; v = prev[v]) {
            cap = min(cap, m[prev[v]][v]);
        }
        for (int v = t; prev[v] >= 0; v = prev[v]) {
            m[prev[v]][v] -= cap;
            m[v][prev[v]] += cap;
        }
        flow += cap;
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
    CHECK(ford_fulkerson(m, 0, 3) == 5);
}

void test2() {
    AMat m = make_mat(4);
    set_edge(m, 0, 1, 3);
    set_edge(m, 1, 3, 1);
    set_edge(m, 0, 2, 1);
    set_edge(m, 2, 3, 2);
    set_edge(m, 2, 1, 1);
    CHECK(ford_fulkerson(m, 0, 3) == 2);
}

void test3() {
    AMat m = make_mat(4);
    set_edge(m, 0, 1, 1);
    set_edge(m, 1, 3, 5);
    set_edge(m, 0, 2, 5);
    set_edge(m, 2, 3, 1);
    set_edge(m, 2, 1, 5);
    CHECK(ford_fulkerson(m, 0, 3) == 6);
}

// This is sample from Wikipedia's page about ford_fulkerson algorithm.
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
    CHECK(ford_fulkerson(m, 0, 5) == 19);
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
