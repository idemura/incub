#include <bits/stdc++.h>

using namespace std;

using AdjList = vector<vector<int>>;

struct Node {
    bool color = false;
    bool black = false;
    Node *up = nullptr, *other = nullptr;
};

struct Edge {
    int i = 0;
    int w = 0;
    int v1i = 0;
    int v2i = 0;
};

Node *get_root(Node *n) {
    auto r = n;
    while (r->up)
        r = r->up;
    while (n->up) {
        auto t = n->up;
        n->up = r;
        n = t;
    }
    return r;
}

// Color is either black or white. We only need to track one of them, because
// we didn't find it in black we assume is white.
struct MSTColor {
    explicit MSTColor(int vertex_n): node(vertex_n) {}

    // Given a set of edges, make minimally weighted MST out of it. We allow
    // edges with ends in vertices of the same color.
    // @es assumed to be sorted from highest to lowest.
    int weighted_color(const vector<Edge> &es, int l, int r) {
        for (auto &e : es) {
            if (!(l <= e.i && e.i <= r)) continue;
            auto n1 = &node[e.v1i];
            auto n2 = &node[e.v2i];
            if (!n1->color && !n2->color) {
                n1->color = true;
                n2->color = true;
                n1->other = n2;
                n2->other = n1;
                n1->black = true; // Make one of two black.
            } else if (!n1->color) {
                n1->color = true;
                n1->up = get_root(n2)->other;
            } else if (!n2->color) {
                n2->color = true;
                n2->up = get_root(n1)->other;
            } else {
                auto r1 = get_root(n1);
                auto r2 = get_root(n2);
                if (r1 == r2) {
                    return e.w;
                } else if (r1->other == r2) {
                    // Go next edge, we are fine.
                } else {
                    // If we connect two different tree, we either connect them
                    // as-is if edge of different colors, of re-paint other
                    // tree.
                    if (r1->black && r2->black) {
                        connect_bb(r1, r2);
                    }
                    if (!r1->black && !r2->black) {
                        connect_bb(r1->other, r2->other);
                    } else {
                        r1->up = r2->other;
                        r1->other->up = r2;
                    }
                }
            }
        }
        return -1;
    }

    // Connect two trees of black: r1 and r2.
    void connect_bb(Node *r1, Node *r2) {
        r1->black = false;
        r1->other->black = true;
        r1->up = r2->other;
        r1->other->up = r2;
    }

    // When we need to allocate a Node for i'th vertex, just take it from
    // this vector at i'th place.
    vector<Node> node;
};

int main() {
    int n = 0, m = 0, q = 0;
    scanf("%d%d%d", &n, &m, &q);
    vector<Edge> es(m);
    int edge_i = 1;
    for (auto &e : es) {
        scanf("%d%d%d", &e.v1i, &e.v2i, &e.w);
        e.v1i--;
        e.v2i--;
        e.i = edge_i++;
    }
    sort(es.begin(), es.end(), [](const Edge &x, const Edge &y) {
        return x.w > y.w;
    });
    for (int i = 0; i < q; i++) {
        int l, r;
        scanf("%d%d\n", &l, &r);
        MSTColor mst(n);
        auto res = mst.weighted_color(es, l, r);
        printf("%d\n", res);
    }
    return 0;
}
