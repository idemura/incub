#include "base.h"

// Min heap for value type of a non-exceptional type T.
template <class T, class C = std::less<T>>
class Heap {
public:
    explicit Heap(C c = C()): cmp(c) {}
    explicit Heap(std::vector<T> data, C c = C()): cmp(c), h(std::move(data)) {
        std::make_heap(h.begin(), h.end(), cmp);
    }

    void push(T v) {
        h.push_back(v);
        std::push_heap(h.begin(), h.end(), cmp);
    }

    bool empty() const {
        return h.empty();
    }
    T top() const {
        return h[0];
    }

    T pop() {
        std::pop_heap(h.begin(), h.end(), cmp);
        auto min = h.back();
        h.pop_back();
        return min;
    }

private:
    struct SwapCmp {
        explicit SwapCmp(C c): c(c) {}
        bool operator()(T a, T b) const {
            return c(b, a);
        }
        C c;
    } cmp;
    std::vector<T> h;
};

template <class C>
struct CmpFirst {
    explicit CmpFirst(C c): c(c) {}
    template <class P>
    bool operator()(const P &a, const P &b) const {
        return c(a.first, b.first);
    }
    C c;
};

// Min heap for value type of a non-exceptional type pair<K, V>.
template <class K, class V, class C = std::less<K>>
class HeapKeyVal: public Heap<std::pair<K, V>, CmpFirst<C>> {
public:
    using Pair = std::pair<K, V>;
    using Base = Heap<Pair, CmpFirst<C>>;
    // `push` overloads are hidden if no this line.
    using Base::push;
    explicit HeapKeyVal(C c = C()): Base(CmpFirst<C>(c)) {}
    explicit HeapKeyVal(std::vector<Pair> data, C c = C()):
            Base(data, CmpFirst<C>(c)) {}
    void push(K k, const V &v) {
        Base::push(make_pair(k, v));
    }
};

struct Edge {
    Edge(int v, int w, int weight): v(v), w(w), weight(weight) {}
    int v = 0;
    int w = 0;
    int weight = 0;
};

i64 mst_prim(
        const vector<Edge> &es, int v_num, vector<Edge> &mst, int start = 0) {
    auto edge_cmp = [](const Edge &a, const Edge &b) { return a.v < b.v; };
    mst.clear();
    i64 length = 0;
    vector<int> in_mst(v_num);
    HeapKeyVal<int, Edge> h;
    for (int v = start; v >= 0;) {
        in_mst[v] = 1;
        for (auto i =
                     lower_bound(es.begin(), es.end(), Edge(v, 0, 0), edge_cmp);
             i != es.end() && i->v == v;
             ++i) {
            if (!in_mst[i->w]) h.push(i->weight, *i);
        }

        v = -1;
        while (!h.empty()) {
            auto e = h.pop();
            if (!in_mst[e.second.w]) {
                v = e.second.w;
                mst.push_back(e.second);
                length += e.first;
                break;
            }
        }
    }
    return length;
}

void mst_prim_prepare_edges(vector<Edge> &es) {
    es.reserve(es.size() * 2);
    for (int i = 0, n = es.size(); i < n; i++) {
        es.emplace_back(es[i].w, es[i].v, es[i].weight);
    }
    auto edge_cmp = [](const Edge &a, const Edge &b) { return a.v < b.v; };
    sort(es.begin(), es.end(), edge_cmp);
}

void test1() {
    // Example from the CLR book.
    vector<Edge> mst,
            es{
                    {0, 1, 4},
                    {0, 2, 8},
                    {1, 2, 11},
                    {1, 4, 8},
                    {2, 3, 7},
                    {3, 4, 2},
                    {3, 5, 6},
                    {2, 5, 1},
                    {5, 7, 2},
                    {4, 7, 4},
                    {4, 6, 7},
                    {6, 7, 14},
                    {6, 8, 9},
                    {7, 8, 10},
            };
    mst_prim_prepare_edges(es);
    CHECK(mst_prim(es, 9, mst, 4) == 37);
    CHECK(mst_prim(es, 9, mst, 0) == 37);
    CHECK(mst.size() == 8);
    for (auto e : mst) {
        cout << "v=" << e.v << " w=" << e.w << " weight=" << e.weight << endl;
    }
}

int main(int argc, char **argv) {
    ios_base::sync_with_stdio(false);
    test1();
    cout << "TESTS PASSED." << endl;
    return 0;
}
