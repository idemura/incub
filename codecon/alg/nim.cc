#include "base.h"

// Keywords: nim game, nimsum, hackenbush, sprague-grundy theorem.
// http://web.mit.edu/sp.268/www/nim.pdf
//
// Handbook of Combinatorics, Volume 1
// By R.L. Graham
// (Google books has in preview about Hackenbush game)

struct Edge {
    int a = -1, b = -1;
    Edge() {}
    Edge(int a, int b): a(a), b(b) {}
};

// @es edges, n - number of vertices.
// @root must be to avoid computing a graph
/*
   4
    \
     3
      \
       1   2
        \ /
         0
*/
// as
/*
   4       2
    \     /
     3   0
      \ /
       1
*/
// (see "root test" for this case).
// Graph must be a tree.
int tree_nimsum(int root, const vector<Edge> &es, int n) {
    // Bit xor and count of adjacent nodes.
    vector<int> adj_xor(n), c(n);
    for (auto e : es) {
        c[e.a]++;
        c[e.b]++;
        adj_xor[e.a] ^= e.b;
        adj_xor[e.b] ^= e.a;
    }
    vector<int> q;
    for (int i = 0; i < c.size(); i++) {
        if (c[i] == 1) {
            q.push_back(i);
        }
    }
    vector<int> v(n);
    for (int i = 0; i < q.size(); i++) {
        if (q[i] == root) continue;
        auto u = adj_xor[q[i]];
        v[u] ^= v[q[i]] + 1; // Add this edge to the stalk we have for q[i].
        adj_xor[u] ^= q[i];
        c[u] -= 1;
        if (c[u] == 1) {
            q.push_back(u);
        }
    }
    return v[root];
}

int count_nodes(const vector<Edge> &es) {
    auto n = 0;
    for (auto e : es) {
        n = max(e.b, max(e.a, n));
    }
    return n + 1;
}

int tree_nimsum(int root, const vector<Edge> &es) {
    return tree_nimsum(root, es, count_nodes(es));
}

int main() {
    CHECK(0 == tree_nimsum(0, {{0, 1}, {0, 2}}));
    CHECK(3 == tree_nimsum(0, {{0, 1}, {0, 2}, {3, 1}}));
    CHECK(0 == tree_nimsum(0, {{0, 1}, {0, 2}, {3, 1}, {4, 2}}));
    CHECK(3 == tree_nimsum(0, {{0, 1}, {0, 2}, {3, 1}, {4, 2}, {1, 5}}));
    // Root test.
    CHECK(2 == tree_nimsum(0, {{0, 1}, {0, 2}, {3, 1}, {3, 4}}));
    CHECK(1 == tree_nimsum(0, {{0, 1}, {0, 2}, {0, 3}}));
    cout << "TESTS PASSED." << endl;
    return 0;
}
