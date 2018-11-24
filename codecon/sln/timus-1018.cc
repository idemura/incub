#include <algorithm>
#include <assert.h>
#include <ctype.h>
#include <functional>
#include <iostream>
#include <limits.h>
#include <list>
#include <map>
#include <math.h>
#include <queue>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <vector>

#define ARRAY_SIZEOF(A) (sizeof(A) / sizeof(A[0]))
#define NON_COPYABLE(C)                                                        \
    C(const C &);                                                              \
    C &operator=(const C &);

using namespace std;

typedef long long int i64;

constexpr int INF = 0x7fffffff;
constexpr int DIM = 108;

struct WeightCount {
    int weight;
    int n;
    WeightCount(): WeightCount(0, 0) {}
    WeightCount(int weight, int n): weight(weight), n(n) {}
};

struct Node {
    int i = 0;
    int weight = 0;
    WeightCount subtree;
    Node *l = nullptr;
    Node *r = nullptr;
    Node(int i, int weight): i(i), weight(weight) {}
};

struct Edge {
    int a = 0;
    int b = 0;
    int weight = 0;
    Edge(int a, int b, int weight): a(a), b(b), weight(weight) {}
};

ostream &operator<<(ostream &s, const Node &node) {
    s << "Node " << node.i << " weight=" << node.weight
      << " subtree weight=" << node.subtree.weight
      << " subnodes=" << node.subtree.n;
    s << " l=";
    if (node.l)
        s << node.l->i;
    else
        s << "null";
    s << " r=";
    if (node.r)
        s << node.r->i;
    else
        s << "null";
    return s;
}

Node *constrcut_tree(int v, int weight, list<Edge> &edges);

Node *pop_edge(list<Edge> &edges, int v) {
    for (auto i = edges.begin(); i != edges.end(); ++i) {
        int w, weight = i->weight;
        if (i->a == v)
            w = i->b;
        else if (i->b == v)
            w = i->a;
        else
            continue;
        edges.erase(i); // O(1)
        return constrcut_tree(w, weight, edges);
    }
    return nullptr;
}

Node *constrcut_tree(int v, int weight, list<Edge> &edges) {
    auto node = new Node(v, weight);
    node->l = pop_edge(edges, v);
    node->r = pop_edge(edges, v);
    return node;
}

bool dp_set[DIM][DIM];
int dp[DIM][DIM];
int cut_min(Node *node, int cuts) {
    if (node == nullptr) {
        return cuts == 0 ? 0 : INF;
    }
    assert(cuts >= 0);
    if (!dp_set[node->i][cuts]) {
        dp_set[node->i][cuts] = true;
        // We cut whole subtree (if cuts == num subnodes) or split cuts to the
        // left and right subtree.
        auto min_weight = INF;
        if (cuts == node->subtree.n) {
            min_weight = node->subtree.weight;
        } else if (node->subtree.n > cuts) {
            for (int c = 0; c <= cuts; c++) {
                auto l = cut_min(node->l, c);
                if (l == INF) continue;
                auto r = cut_min(node->r, cuts - c);
                if (r == INF) continue;
                min_weight = min(min_weight, l + r);
            }
        }
        dp[node->i][cuts] = min_weight;
    }
    return dp[node->i][cuts];
}

WeightCount count_subtree(Node *node) {
    if (node == nullptr) {
        return WeightCount(0, 0);
    } else {
        auto wc_l = count_subtree(node->l);
        auto wc_r = count_subtree(node->r);
        node->subtree = WeightCount(
                wc_l.weight + wc_r.weight + node->weight, wc_l.n + wc_r.n + 1);
        return node->subtree;
    }
}

void print_tree(Node *node) {
    if (node != nullptr) {
        cout << *node << endl;
        print_tree(node->l);
        print_tree(node->r);
    }
}

int main(int argc, char **argv) {
    int n = 0, q = 0;
    cin >> n >> q;

    list<Edge> edges;
    auto total = 0;
    for (auto i = 1; i < n; i++) {
        int a, b, weight;
        cin >> a >> b >> weight;
        edges.push_back(Edge(a, b, weight));
        total += weight;
    }

    auto root = constrcut_tree(1, 0, edges);
    count_subtree(root);
    // print_tree(root);
    cout << total - cut_min(root, n - 1 - q) << endl;
    return 0;
}
