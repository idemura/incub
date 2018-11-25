#include "base.h"

using namespace std;

void print_matrix(const vector<vector<int>> &m, const string &tag) {
    cout << tag << ":\n";
    for (const auto &r : m) {
        if (r.empty()) {
            cout << "<empty>";
        } else {
            for (auto x : r) {
                cout << x << " ";
            }
        }
        cout << "\n";
    }
}

void print_vector(const vector<int> &r, const string &tag) {
    cout << tag << ": ";
    for (auto x : r) {
        cout << x << " ";
    }
    cout << "\n";
}

vector<vector<int>> make_matrix(int n) {
    vector<vector<int>> m(n);
    for (auto &r : m)
        r.resize(m.size());
    return m;
}

// Allowed to push of height (h - 1) where h is the height of the current
// top.
class PushRelabelQ {
public:
    explicit PushRelabelQ(int h_max): q(h_max), not_empty(h_max) {}

    void init(int v) {
        q[0].push_back(v);
    }

    int get_top() {
        return q[top].back();
    }

    // Relabels top.
    void relabel() {
        auto v = q[top].back();
        q[top].pop_back();
        if (q[top].empty()) {
            not_empty[top + 1] = not_empty[top];
        } else {
            not_empty[top + 1] = top;
        }
        top++;
        q[top].push_back(v);
    }

    // Do this before @pop.
    void add_downhill(int v) {
        q[top - 1].push_back(v);
        // `top - 1` may be empty, meaning that `not_empty[top]` points for
        // something way lower.
        not_empty[top] = top - 1;
    }

    void pop() {
        q[top].pop_back();
        if (q[top].empty()) {
            top = not_empty[top];
        }
    }

    bool empty() const {
        return top == 0 && q[top].empty();
    }

    void print(ostream &os) const {
        os << "PushRelabelQ (" << q.size() << " level(s)):\n";
        for (int i = 0; i < q.size(); i++) {
            if (i == top) {
                cout << "> ";
            } else {
                cout << "  ";
            }
            cout << "h=" << i << " not_empty=" << not_empty[i] << ": ";
            if (q[i].empty()) {
                cout << "<empty>";
            } else {
                for (auto x : q[i]) {
                    cout << x << " ";
                }
            }
            cout << "\n";
        }
    }

private:
    int top = 0;
    vector<vector<int>> q;
    vector<int> not_empty;
};

ostream &operator<<(ostream &os, const PushRelabelQ &q) {
    q.print(os);
    return os;
}

vector<vector<int>>
push_relabel(const vector<vector<int>> &c_in, int s, int t) {
    auto c = c_in;
    const auto n = c.size();
    auto f = make_matrix(n);
    vector<int> h(n);
    h[s] = n;
    PushRelabelQ q(2 * n);
    vector<int> e(n);
    vector<vector<int>> downhill(n);
    for (int i = 0; i < n; i++) {
        if (c[s][i] > 0) {
            e[i] = f[s][i] = c[s][i];
            c[s][i] = 0;
            if (i != t) q.init(i);
        }
    }

    while (!q.empty()) {
        auto v = q.get_top();
        if (downhill[v].empty()) {
            // Relabel
            q.relabel();
            h[v]++;
            downhill[v].clear();
            for (int j = 0; j < n; j++) {
                if (h[j] < h[v] && c[v][j] + f[j][v] > 0) {
                    downhill[v].push_back(j);
                }
            }
        } else {
            // Push
            auto w = downhill[v].back();
            downhill[v].pop_back();
            if (e[w] == 0 && w != s && w != t) {
                q.add_downhill(w);
            }
            auto d = min(e[v], f[w][v]);
            e[w] += d;
            e[v] -= d;
            c[w][v] += d;
            f[w][v] -= d;
            if (e[v] > 0) {
                d = min(e[v], c[v][w]);
                e[w] += d;
                e[v] -= d;
                c[v][w] -= d;
                f[v][w] += d;
            }
            if (e[v] == 0) {
                downhill[v].clear();
                q.pop();
            }
        }
    }
    return f;
}

// Gets highest vertex with excess other than @s and @t.
int get_vertex(const vector<int> &h, const vector<int> &e, int s, int t) {
    int h_max = -1;
    int j = -1;
    for (int i = 0; i < h.size(); i++) {
        if (e[i] > 0 && i != s && i != t && h[i] > h_max) {
            h_max = h[i];
            j = i;
        }
    }
    return j;
}

int get_vertex_downhill(
        int v,
        const vector<int> &h,
        const vector<vector<int>> &c,
        const vector<vector<int>> &f) {
    for (int i = 0; i < h.size(); i++) {
        if (h[i] < h[v] && c[v][i] + f[i][v] > 0) {
            return i;
        }
    }
    return -1;
}

vector<vector<int>>
push_relabel_correct(const vector<vector<int>> &c_in, int s, int t) {
    auto c = c_in;
    const auto n = c.size();
    auto f = make_matrix(n);
    vector<int> h(n);
    h[s] = n;
    vector<int> e(n);
    // Full flow on edges from @s.
    for (int i = 0; i < n; i++) {
        if (c[s][i] > 0) {
            e[i] = f[s][i] = c[s][i];
            c[s][i] = 0;
        }
    }

    for (;;) {
        int v = get_vertex(h, e, s, t);
        if (v < 0) {
            break;
        }
        int w = get_vertex_downhill(v, h, c, f);
        if (w >= 0) {
            if (e[v] <= f[w][v]) {
                f[w][v] -= e[v];
                c[w][v] += e[v];
                e[w] += e[v];
                e[v] = 0;
            } else {
                auto d = min(e[v], c[v][w] + f[w][v]);
                e[v] -= d;
                e[w] += d;
                d -= f[w][v];
                c[v][w] -= d;
                f[v][w] += d;
                c[w][v] += f[w][v];
                f[w][v] = 0;
            }
        } else {
            h[v]++;
        }
    }
    return f;
}

int get_flow(const vector<int> &fv) {
    int flow = 0;
    for (auto x : fv) {
        flow += x;
    }
    return flow;
}

map<pair<int, int>, int> flow_map(const vector<vector<int>> &f) {
    map<pair<int, int>, int> m;
    for (int i = 0; i < f.size(); i++) {
        for (int j = 0; j < f.size(); j++) {
            if (f[i][j] > 0) {
                m[make_pair(i, j)] = f[i][j];
            }
        }
    }
    return m;
}

void print_flow_map(const map<pair<int, int>, int> &m) {
    for (auto e : m) {
        cout << e.first.first << "," << e.first.second << " - " << e.second
             << "\n";
    }
}

using push_relabel_fn = function<vector<vector<int>>(
        const vector<vector<int>> &c, int s, int t)>;

void test1(push_relabel_fn push_relabel) {
    auto m = make_matrix(4);
    m[0][1] = 1;
    m[0][2] = 4;
    m[1][2] = 5;
    m[2][1] = 2;
    m[1][3] = 4;
    m[2][3] = 1;
    auto f = push_relabel(m, 0, m.size() - 1);
    CHECK(4 == get_flow(f[0]));
    auto f_map = flow_map(f);
    CHECK(5 == f_map.size());
    CHECK(1 == f_map[make_pair(0, 1)]);
    CHECK(3 == f_map[make_pair(0, 2)]);
    CHECK(2 == f_map[make_pair(2, 1)]);
    CHECK(1 == f_map[make_pair(2, 3)]);
    CHECK(3 == f_map[make_pair(1, 3)]);
}

void test2(push_relabel_fn push_relabel) {
    auto m = make_matrix(2);
    m[0][1] = 3;
    auto f = push_relabel(m, 0, m.size() - 1);
    CHECK(3 == get_flow(f[0]));
    auto f_map = flow_map(f);
    CHECK(1 == f_map.size());
    CHECK(3 == f_map[make_pair(0, 1)]);
}

void test3(push_relabel_fn push_relabel) {
    auto m = make_matrix(5);
    m[0][1] = 1;
    m[0][2] = 4;
    m[1][2] = 5;
    m[2][1] = 2;
    m[1][3] = 4;
    m[2][3] = 1;
    m[3][4] = 3;
    m[4][3] = 1;
    auto f = push_relabel(m, 0, m.size() - 1);
    CHECK(3 == get_flow(f[0]));
    auto f_map = flow_map(f);
    CHECK(5 == f_map.size());
    if (f_map.end() != f_map.find(make_pair(0, 1))) {
        CHECK(1 == f_map[make_pair(0, 1)]);
        CHECK(2 == f_map[make_pair(0, 2)]);
        CHECK(2 == f_map[make_pair(2, 1)]);
        CHECK(3 == f_map[make_pair(1, 3)]);
    } else {
        CHECK(3 == f_map[make_pair(0, 2)]);
        CHECK(2 == f_map[make_pair(2, 1)]);
        CHECK(1 == f_map[make_pair(2, 3)]);
        CHECK(2 == f_map[make_pair(1, 3)]);
    }
    CHECK(3 == f_map[make_pair(3, 4)]);
}

void test4(push_relabel_fn push_relabel) {
    auto m = make_matrix(5);
    m[0][1] = 4;
    m[1][2] = 1;
    m[1][3] = 2;
    m[3][4] = 2;
    m[2][4] = 2;
    auto f = push_relabel(m, 0, m.size() - 1);
    CHECK(3 == get_flow(f[0]));
    auto f_map = flow_map(f);
    CHECK(5 == f_map.size());
    CHECK(3 == f_map[make_pair(0, 1)]);
    CHECK(1 == f_map[make_pair(1, 2)]);
    CHECK(2 == f_map[make_pair(1, 3)]);
    CHECK(1 == f_map[make_pair(2, 4)]);
    CHECK(2 == f_map[make_pair(3, 4)]);
}

void test5(push_relabel_fn push_relabel) {
    auto m = make_matrix(6);
    m[0][1] = 16;
    m[0][2] = 13;
    m[1][2] = 10;
    m[1][3] = 12;
    m[2][1] = 4;
    m[2][4] = 14;
    m[3][2] = 9;
    m[3][5] = 20;
    m[4][3] = 7;
    m[4][5] = 4;
    auto f = push_relabel(m, 0, m.size() - 1);
    CHECK(23 == get_flow(f[0]));
    auto f_map = flow_map(f);
    CHECK(8 == f_map.size());
    CHECK(13 == f_map[make_pair(0, 1)]);
    CHECK(10 == f_map[make_pair(0, 2)]);
    CHECK(1 == f_map[make_pair(1, 2)]);
    CHECK(12 == f_map[make_pair(1, 3)]);
    CHECK(11 == f_map[make_pair(2, 4)]);
    CHECK(19 == f_map[make_pair(3, 5)]);
    CHECK(7 == f_map[make_pair(4, 3)]);
    CHECK(4 == f_map[make_pair(4, 5)]);
}

void tests(push_relabel_fn pr) {
    test1(pr);
    test2(pr);
    test3(pr);
    test4(pr);
    test5(pr);
}

int main() {
    tests(&push_relabel_correct);
    tests(&push_relabel);
    cout << "TESTS PASSED." << endl;
    return 0;
}
