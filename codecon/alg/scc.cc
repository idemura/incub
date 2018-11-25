#include "base.h"

using VertexVec = vector<int>;
using Edge = pair<int, int>;

struct SccTarjanCtx {
    SccTarjanCtx(const vector<VertexVec> &a): a(a) {
        i.resize(a.size(), -1);
        r.resize(a.size(), -1);
        on_stack.resize(a.size());
        scc.resize(a.size());
    }

    const vector<VertexVec> &a;
    vector<int> scc;
    int scc_num = 0;
    int vertex_counter = 0;
    vector<int> i;
    vector<int> on_stack;
    vector<int> s; // Stack.
    vector<int> r; // Root index (low link).
    NON_COPYABLE(SccTarjanCtx);
};

void dfs_tarjan(SccTarjanCtx &c, int v) {
    c.i[v] = c.vertex_counter++;
    c.r[v] = c.i[v];
    c.on_stack[v] = 1;
    c.s.push_back(v);

    for (auto w : c.a[v]) {
        if (c.i[w] < 0) {
            dfs_tarjan(c, w);
            c.r[v] = min(c.r[v], c.r[w]);
        } else if (c.on_stack[w]) {
            c.r[v] = min(c.r[v], w);
        }
    }

    if (c.r[v] == c.i[v]) {
        for (int w = -1; w != v;) {
            w = c.s.back();
            c.s.pop_back();
            c.on_stack[w] = 0;
            c.scc[w] = c.scc_num;
        }
        c.scc_num++;
    }
}

// A good explanation at Wikipedia:
// http://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm
void scc_tarjan(const vector<VertexVec> &a, vector<int> &scc, int &scc_num) {
    SccTarjanCtx c(a);
    for (int i = 0; i < c.a.size(); i++) {
        if (c.i[i] < 0) dfs_tarjan(c, i);
    }
    scc = move(c.scc);
    scc_num = c.scc_num;
}

vector<Edge> scc_reduce(const vector<VertexVec> &a, const vector<int> &scc) {
    set<Edge> edge_set;
    for (int v = 0; v < a.size(); v++) {
        for (auto w : a[v]) {
            auto e = Edge(scc[v], scc[w]);
            if (e.first != e.second) edge_set.insert(e);
        }
    }
    return vector<Edge>(edge_set.begin(), edge_set.end());
}

vector<VertexVec> scc_to_groups(const vector<int> &scc, int k) {
    vector<VertexVec> groups(k);
    for (int i = 0; i < scc.size(); i++) {
        groups[scc[i]].push_back(i);
    }
    for (auto &g : groups) {
        sort(g.begin(), g.end());
    }
    sort(groups.begin(),
         groups.end(),
         [](const VertexVec &a, const VertexVec &b) { return a[0] < b[0]; });
    return move(groups);
}

template <class T>
bool vector_eq(const vector<T> &a, const vector<T> &b) {
    return a == b;
}

template <class T>
bool vector_sort_eq(vector<T> a, vector<T> b) {
    sort(a.begin(), a.end());
    sort(b.begin(), b.end());
    return vector_eq(a, b);
}

void test1() {
    vector<VertexVec> a{{1}, {2}, {0}, {0, 2}};
    vector<int> scc;
    int scc_num = 0;
    scc_tarjan(a, scc, scc_num);

    auto gs = scc_to_groups(scc, scc_num);
    CHECK(gs.size() == 2);
    CHECK(vector_eq(gs[0], {0, 1, 2}));
    CHECK(vector_eq(gs[1], {3}));

    auto es = scc_reduce(a, scc);
    CHECK(vector_sort_eq(es, {{1, 0}}));
}

void test2() {
    vector<VertexVec> a{{1, 3}, {2}, {1}, {4}, {5}, {3}};
    vector<int> scc;
    int scc_num = 0;
    scc_tarjan(a, scc, scc_num);

    auto gs = scc_to_groups(scc, scc_num);
    CHECK(gs.size() == 3);
    CHECK(vector_eq(gs[0], {0}));
    CHECK(vector_eq(gs[1], {1, 2}));
    CHECK(vector_eq(gs[2], {3, 4, 5}));

    auto es = scc_reduce(a, scc);
    CHECK(vector_sort_eq(es, {{2, 0}, {2, 1}}));
}

void test3() {
    vector<VertexVec> a{{1, 2}, {2}, {3}, {0}};
    vector<int> scc;
    int scc_num = 0;
    scc_tarjan(a, scc, scc_num);

    auto gs = scc_to_groups(scc, scc_num);
    CHECK(gs.size() == 1);
    CHECK(vector_eq(gs[0], {0, 1, 2, 3}));

    auto es = scc_reduce(a, scc);
    CHECK(vector_sort_eq(es, {}));
}

int main() {
    ios_base::sync_with_stdio(false);
    test1();
    test2();
    test3();
    cout << "TESTS PASSED." << endl;
    return 0;
}
