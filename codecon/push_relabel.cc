#include "base.h"

using namespace std;

bool FLAG_step_mode = false;

/*
Seems like relabel happens until vertex has no excess or all edges saturated,
because it only goes up.
*/

void print_matrix(const vector<vector<int>> &m, const string &tag) {
    cout<<tag<<":\n";
    for (const auto &r : m) {
        if (r.empty()) {
            cout<<"<empty>";
        } else {
            for (auto x : r) {
                cout<<x<<" ";
            }
        }
        cout<<"\n";
    }
}

void print_vector(const vector<int> &r, const string &tag) {
    cout<<tag<<": ";
    for (auto x : r) {
        cout<<x<<" ";
    }
    cout<<"\n";
}

void wait_enter() {
    if (FLAG_step_mode) {
        cout<<"Press Enter... ";
        string s;
        getline(cin, s);
    }
}

vector<vector<int>> make_matrix(int n) {
    vector<vector<int>> m(n);
    for (auto &r : m) r.resize(m.size());
    return m;
}

// Allowed to push of height (h - 1) where h is the height of the current
// top.
class PushRelabelQ {
  public:
    explicit PushRelabelQ(int h_max)
            : q(h_max),
              not_empty(h_max) {
    }

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
        // `top - 1` may be empty, meaning that `not_empty[top]` points for something way lower.
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
        os<<"PushRelabelQ ("<<q.size()<<" level(s)):\n";
        for (int i = 0; i < q.size(); i++) {
            if (i == top) {
                cout<<"> ";
            } else {
                cout<<"  ";
            }
            cout<<"h="<<i<<" not_empty="<<not_empty[i]<<": ";
            if (q[i].empty()) {
                cout<<"<empty>";
            } else {
                for (auto x : q[i]) {
                    cout<<x<<" ";
                }
            }
            cout<<"\n";
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

// @c_in is matrix of capacities.
vector<vector<int>> push_relabel(
        const vector<vector<int>> &c_in,
        int s,
        int t) {
    auto c = c_in;
    cout<<"s="<<s<<" t="<<t<<endl;
    print_matrix(c, "c");
    const auto n = c.size();
    auto f = make_matrix(n);
    // We can undo flow excess back where it came from.
    //vector<vector<int>> h(2 * n);
    // Height of each vertex.
    vector<int> h(n);
    h[s] = n;
    // Queue of vertices with excess by height. Highest - last.
    PushRelabelQ q(2 * n);
    //int h_top = 0;
    // Maps height to previous height.
    //vector<int> prev(2 * n, -1);
    //h[s] = n;
    vector<int> e(n);
    cout<<"__ 1a"<<endl;
    // Full flow on edges from @s.
    vector<vector<int>> downhill(n);
    //vector<int> hprev(n, -1);
    for (int i = 0; i < n; i++) {
        if (c[s][i] > 0) {
            e[i] = f[s][i] = c[s][i];
            //c[i][s] += f[s][i];    // There can be an arc back already.
            //b[i][s] += f[s][i];
            c[s][i] = 0;
            if (i != t) q.init(i);
        }
    }
    //print_matrix(b, "initial b");

    // cout<<"__ 1"<<endl;
    while (!q.empty()) {
        wait_enter();
        cout<<endl;
        print_vector(e, "excess");
        //cout<<"h_top="<<h_top<<endl;
        auto v = q.get_top();
        cout<<q;
        print_vector(h, "h");
        cout<<"pick vertex v="<<v<<" <---------------"<<endl;
        print_vector(downhill[v], "downhill");
        if (downhill[v].empty()) {
            // Relabel
            cout<<"relabel v="<<v<<endl;
            q.relabel();
            h[v]++;
            cout<<"new h="<<h[v]<<endl;
            downhill[v].clear();
            for (int j = 0; j < n; j++) {
                if (h[j] < h[v] && c[v][j] + f[j][v] > 0) {
                    cout<<"added "<<j<<endl;
                    downhill[v].push_back(j);
                } else {
                    cout<<j<<" is not downhill: "<<h[j]<<endl;
                }
            }
            cout<<"end downhill"<<endl;
            //h[v]++;
            print_vector(h, "h");
            //q.push_back(v);
            //q.add_front(v);
        } else {
            //cout<<"push"<<endl;
            // Push
            auto w = downhill[v].back();
            cout<<"push to w="<<w<<endl;
            downhill[v].pop_back();
            // Cancel some flow to the vertex and use new capacity.
            //auto d = min(c[v][w] + b[v][w], e[v]);
            cout<<"back flow "<<w<<" to "<<v<<": "<<f[w][v]<<endl;
            auto d = min(e[v], f[w][v]);
            cout<<"d="<<d<<" e[v]="<<e[v]<<" c+f="<<(c[v][w] + f[w][v])<<endl;
            if (e[w] == 0 && w != s && w != t) {
                q.add_downhill(w);
            }
            //auto no_excess_w = e[w] == 0;
            e[w] += d;
            e[v] -= d;
            c[w][v] += d;
            f[w][v] -= d;
            if (e[v] > 0) {
                d = min(e[v], c[v][w]);
                cout<<"need to use capacity d="<<d<<endl;
                e[w] += d;
                e[v] -= d;
                c[v][w] -= d;
                f[v][w] += d;
            }
            if (e[v] == 0) {
                cout<<"no more excess, downhill cleared"<<endl;
                downhill[v].clear();
                q.pop();
            } else {
                cout<<"leave in q"<<endl;
                //q.add_front(v);
                //q.push_back(v);
            }
            // print_matrix(c, "c");
            // print_matrix(f, "f");
            // print_vector(e, "e");
        }
        cout<<"new q:\n"<<q;
        cout<<"empty="<<q.empty()<<endl;
    }
    return f;
}

// Gets highest vertex with excess other than @s and @t.
int get_vertex(const vector<int> &h, const vector<int> &e, int s, int t) {
    cout<<"get_vertex\n";
    print_vector(h, "h");
    print_vector(e, "e");
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
    cout<<"get_vertex_downhill from v="<<v<<"\n";
    print_vector(h, "h");
    //print_vector(c, "c");
    //print_vector(b, "b");
    for (int i = 0; i < h.size(); i++) {
        if (h[i] < h[v] && c[v][i] + f[i][v] > 0) {
            return i;
        }
    }
    return -1;
}

vector<vector<int>> push_relabel_correct(
        const vector<vector<int>> &c_in,
        int s,
        int t) {
    auto c = c_in;
    cout<<"s="<<s<<" t="<<t<<endl;
    print_matrix(c, "c");
    const auto n = c.size();
    auto f = make_matrix(n);
    // We can undo flow excess back where it came from.
    vector<int> h(n);
    h[s] = n;
    vector<int> e(n);
    cout<<"__ 1a"<<endl;
    // Full flow on edges from @s.
    for (int i = 0; i < n; i++) {
        if (c[s][i] > 0) {
            e[i] = f[s][i] = c[s][i];
            //c[i][s] += f[s][i];    // There can be an arc back already.
            //b[i][s] += f[s][i];
            c[s][i] = 0;
        }
    }
    //print_matrix(b, "initial b");

    // cout<<"__ 1"<<endl;
    for (;;) {
        wait_enter();
        cout<<endl;
        // cout<<"__ 2"<<endl;
        int v = get_vertex(h, e, s, t);
        if (v < 0) {
            cout<<"flow done"<<endl;
            break;
        }
        cout<<"pick vertex v="<<v<<endl;
        cout<<"consider h="<<h[v]<<endl;
        int w = get_vertex_downhill(v, h, c, f);
        cout<<"downhill to w="<<w<<endl;
        if (w >= 0) {
            // Push
            // Cancel some flow to the vertex and use new capacity.
            //auto d = min(c[v][w] + b[v][w], e[v]);
            cout<<"flow to my v from w: "<<f[w][v]<<endl;
            if (e[v] <= f[w][v]) {
                cout<<"flow to me is more than excess\n";
                f[w][v] -= e[v];
                c[w][v] += e[v];
                e[w] += e[v];
                e[v] = 0;
            } else {
                cout<<"need to use capacity\n";
                auto d = min(e[v], c[v][w] + f[w][v]);
                cout<<"d to move: "<<d<<endl;
                e[v] -= d;
                e[w] += d;
                d -= f[w][v];
                c[v][w] -= d;
                f[v][w] += d;
                c[w][v] += f[w][v];
                f[w][v] = 0;
            }
            //auto d = min(c[v][w] + f[w][v], e[v]);
            //e[v] -= d;
            //e[w] += d;
            //d = min(d, b[v][w]);
            //cout<<"push "<<d<<"\n";
            //cout<<"c: "<<c[v][w]<<" b: "<<b[v][w]<<endl;
            //f[v][w] += d;
            //print_matrix(b, "b -- initial state");
            //cout<<"d_min: "<<d_min<<endl;
            //b[v][w] -= d_min;
            //print_matrix(b, "b -- reduce forward excess");
            //c[v][w] -= d;
            //b[w][v] += d;
            //print_matrix(b, "b -- add back excess flow");
            // if (c[w][v] > c_in[w][v]) {
            //     cout<<"cut by flow\n";
            //     c[w][v] = c_in[w][v];
            // }
            print_matrix(c, "c");
            print_matrix(f, "f");
            print_vector(e, "e");
        } else {
            // Relabel
            cout<<"relabel "<<v<<endl;
            h[v]++;
            print_vector(h, "h");
        }
    }
    print_matrix(f, "final flow");
    //auto full_flow = 0;
    //for (int i = 0; i < n; i++) {
        //full_flow += f[s][i];
    //}
    //cout<<"full_flow="<<full_flow<<endl;
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

using push_relabel_fn = function<vector<vector<int>>(
    const vector<vector<int>> &c,
    int s,
    int t)>;

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
    cout<<"FLOW: "<<get_flow(f[0])<<"\n";
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

int main() {
    FLAG_step_mode = false;
    const auto pr = push_relabel_fn(&push_relabel);
    test1(pr);
    test2(pr);
    test3(pr);
    test4(pr);
    cout << "TESTS PASSED." << endl;
    return 0;
}
