#include "base.h"

template <class T>
using vecvec = vector<vector<T>>;

template <class T, class Less = less<T>>
class FCascade {
public:
    using ValueT = T;

    explicit FCascade(Less le = Less()): le_(le) {}
    DEFAULT_COPY(FCascade);

    void clear() {
        cascades_.clear();
    }
    void add_list(const vector<ValueT> &a);
    // Returns list of @x lower bound indices.
    vector<int> search(ValueT x) const;
    void print() const;

private:
    static constexpr int kBlack = 0;
    static constexpr int kWhite = 1;

    // I call nodes from the i-th list 'white' and nodes cascaded from (i-1)-th
    // level 'black'.
    struct Node {
        ValueT v = ValueT(); // Value.
        int c = kBlack; // Color.
        // Index in the original list. For black, index in the original list of
        // the white node to the right.
        int j = -1;
        // Index in the cascade below.
        int cascade_j = 0;

        ostream &print(ostream &os) const {
            return os << "{color=" << (c == kWhite ? "white" : "black")
                      << " v=" << v << " j=" << j << " cascade_j=" << cascade_j
                      << "}";
        }
    };

    void build_cascade(
            const vector<ValueT> &w,
            const vector<Node> &b,
            vector<Node> &res) const;
    int binary(const vector<Node> &nl, ValueT x) const;
    bool lt(ValueT a, ValueT b) const {
        return le_(a, b);
    }
    bool gt(ValueT a, ValueT b) const {
        return le_(b, a);
    }
    bool le(ValueT a, ValueT b) const {
        return !gt(a, b);
    }
    bool ge(ValueT a, ValueT b) const {
        return !lt(a, b);
    }

    // In each cascade, white nodes go first because of lower_bound property.
    list<vector<Node>> cascades_;
    const Less le_;
};

template <class T, class Less>
void FCascade<T, Less>::build_cascade(
        const vector<T> &w, const vector<Node> &b, vector<Node> &res) const {
    res.resize(w.size() + (b.size() + 1) / 2);
    int wi = 0, bi = 0, ri = 0;
    int cascade_j = -1;
    for (; bi < b.size() && wi < w.size(); ri++) {
        CHECK(ri < res.size());
        if (lt(b[bi].v, w[wi])) {
            res[ri].v = b[bi].v;
            res[ri].c = kBlack;
            res[ri].cascade_j = bi;
            bi += 2;
        } else {
            res[ri].v = w[wi];
            res[ri].c = kWhite;
            res[ri].j = wi;
            wi += 1;
        }
    }
    for (; wi < w.size(); wi += 1, ri++) {
        CHECK(ri < res.size());
        res[ri].v = w[wi];
        res[ri].c = kWhite;
        res[ri].j = wi;
    }
    for (; bi < b.size(); bi += 2, ri++) {
        CHECK(ri < res.size());
        res[ri].v = b[bi].v;
        res[ri].c = kBlack;
        res[ri].cascade_j = bi;
    }
    CHECK(res.size() == ri);
    // Setup "right" indices.
    cascade_j = b.size();
    int j = w.size();
    for (int i = res.size(); i-- > 0;) {
        if (res[i].c == kBlack) {
            res[i].j = j;
            cascade_j = res[i].cascade_j;
        } else {
            res[i].cascade_j = cascade_j;
            j = res[i].j;
        }
    }
}

// Builds fixed fraction cascading (fraction parameter p = 1/2).
template <class T, class Less>
void FCascade<T, Less>::add_list(const vector<T> &a) {
    if (cascades_.empty()) {
        cascades_.emplace_front(a.size());
        auto &nl = cascades_.front();
        for (int i = 0; i < a.size(); i++) {
            nl[i].v = a[i];
            nl[i].c = kWhite;
            nl[i].j = i;
        }
    } else {
        auto &nl_prev = cascades_.front();
        cascades_.emplace_front();
        build_cascade(a, nl_prev, cascades_.front());
    }
}

template <class T, class Less>
int FCascade<T, Less>::binary(const vector<Node> &nl, T x) const {
    auto i = lower_bound(
            nl.begin(), nl.end(), x, [this](const Node &node, const T &y) {
                return lt(node.v, y);
            });
    return i - nl.begin();
}

// Returns vector of list index (first) and index in the list (second) where
// `n` where found.
template <class T, class Less>
vector<int> FCascade<T, Less>::search(T x) const {
    vector<int> loc(cascades_.size()); // For RVO.
    if (cascades_.empty()) {
        return loc;
    }
    int index = 1, i = -1;
    for (auto &nl : cascades_) {
        if (index == 1) i = binary(nl, x);
        if (i < 0) i = nl.size();
        // Minimum is always first and cascaded up. So if x less than first we
        // can break (@loc initialized with 0).
        if (i == 0 && lt(x, nl[i].v)) break;
        if (i != 0 && le(x, nl[i - 1].v)) {
            i--;
        }
        if (i == nl.size()) {
            loc[cascades_.size() - index] = nl.back().j + 1;
            i = -1; // Set i to size of the next cascade on next iteration.
        } else {
            loc[cascades_.size() - index] = nl[i].j;
            i = nl[i].cascade_j;
        }
        index++;
    }
    return loc;
}

template <class T, class Less>
void FCascade<T, Less>::print() const {
    int l = 0;
    cout << cascades_.size()
         << " fractional cascades in reverse order:" << endl;
    for (auto &nl : cascades_) {
        cout << "Cascade #" << (l++) << endl;
        for (int i = 0; i < nl.size(); i++) {
            cout << "  Node #" << i << " ";
            nl[i].print(cout) << endl;
        }
    }
}

void print_search_result(int x, const vector<int> &res) {
    cout << "Search " << x << " result:" << endl;
    for (int i = 0; i < res.size(); i++) {
        cout << "  list " << i << " #" << res[i] << endl;
    }
}

bool check_fc(const vecvec<int> &l, int x, const vector<int> &res) {
    // print_search_result(x, res);
    for (int i = 0; i < l.size(); i++) {
        int lb = lower_bound(l[i].begin(), l[i].end(), x) - l[i].begin();
        if (lb != res[i]) {
            cout << "Mismatch in #" << i << " x=" << x << ": " << res[i]
                 << " expected " << lb << "\n";
            cout << "List:\n";
            for (auto e : l[i])
                cout << e << " ";
            cout << endl;
            return false;
        }
    }
    return true;
}

void test1() {
    const vecvec<int> l{
            {1, 10, 20},
            {5, 9, 15, 24},
    };
    FCascade<int> fc;
    for (auto &v : l)
        fc.add_list(v);
    // fc.print();
    for (int x = 0; x <= 25; x++) {
        CHECK(check_fc(l, x, fc.search(x)));
    }
}

void test2() {
    const vecvec<int> l{
            {1, 10, 20, 29},
            {5, 9, 15, 24},
            {2, 8, 10, 16, 24},
    };
    FCascade<int> fc;
    for (auto &v : l)
        fc.add_list(v);
    // fc.print();
    for (int x = 0; x <= 30; x++) {
        CHECK(check_fc(l, x, fc.search(x)));
    }
}

int main(int argc, char **argv) {
    ios_base::sync_with_stdio(false);
    test1();
    test2();
    cout << "TESTS PASSED." << endl;
    return 0;
}
