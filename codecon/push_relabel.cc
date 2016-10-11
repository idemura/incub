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
    for (auto x : r) {
      cout<<x<<" ";
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
    string s;
    getline(cin, s);
  }
}

vector<vector<int>> make_matrix(int n) {
  vector<vector<int>> m(n);
  for (auto &r : m) r.resize(m.size());
  return m;
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
  vector<vector<int>> h(2 * n);
  int h_top = 0;
  // Maps height to previous height.
  vector<int> prev(2 * n, -1);
  //h[s] = n;
  vector<int> e(n);
  cout<<"__ 1a"<<endl;
  // Full flow on edges from @s.
  vector<vector<int>> downhill(n);
  //vector<int> hprev(n, -1);
  for (int i = 0; i < n; i++) {
    if (c[s][i] > 0) {
      e[i] = f[s][i] = c[s][i];
      //c[i][s] += f[s][i];  // There can be an arc back already.
      //b[i][s] += f[s][i];
      c[s][i] = 0;
      h[0].push_back(i);
    }
  }
  //print_matrix(b, "initial b");

  // cout<<"__ 1"<<endl;
  for (;;) {
    wait_enter();
    cout<<endl;
    if (h_top < 0) {
      cout<<"flow done"<<endl;
      break;
    }
    cout<<"h_top="<<h_top<<endl;
    auto v = h[h_top].back();
    print_vector(h[h_top], "h_top list");
    cout<<"pick vertex v="<<endl;
    h[h_top].pop_back();
    if (h[h_top].empty()) {
      print_vector(prev, "goto prev");
      h_top = prev[h_top];
      cout<<"new h_top="<<h_top<<endl;
    }
    print_vector(downhill[v], "downhill");
    if (downhill[v].empty()) {
      // Relabel
      cout<<"relabel "<<v<<endl;
      h_top++;
      cout<<"increase h_top"<<h_top<<endl;
      h[h_top].push_back(v);
      print_vector(h[h_top], "h_top new");
      prev[h_top] = prev[h_top - 1];
      print_vector(h[h_top - 1], "check for new downhill in");
      for (int j = 0; j < h[h_top - 1].size(); j++) {
        if (c[v][j] + f[j][v] > 0) {
          cout<<"new flow "<<(c[v][j] + f[j][v])<<endl;
          downhill[v].push_back(j);
        }
      }
      //h[v]++;
      print_matrix(h, "h");
    } else {
      cout<<"push"<<endl;
      // Push
      auto w = downhill[v].back();
      cout<<"push to w="<<w<<endl;
      downhill[v].pop_back();
      // Cancel some flow to the vertex and use new capacity.
      //auto d = min(c[v][w] + b[v][w], e[v]);
      cout<<"flow to my v from w: "<<f[w][v]<<endl;
      if (e[v] < f[w][v]) {
        cout<<"flow to me is more than excess\n";
        f[w][v] -= e[v];
        c[w][v] += e[v];
        e[w] += e[v];
        e[v] = 0;
        cout<<"clear downhill"<<endl;
        downhill[v].clear();
      } else {
        cout<<"need to use capacity\n";
        auto d = min(e[v], c[v][w] + f[w][v]);
        if (e[v] > d) {
          cout<<"excess is not empty, return v"<<endl;
          h[h_top].push_back(v);
        } else {
          //downhill[v].push_back(w);
          // clear downhill actually
          cout<<"excess is 0"<<endl;
          downhill[v].clear();
        }
        cout<<"d to move: "<<d<<endl;
        e[w] += d;
        e[v] -= d;
        d -= f[w][v];
        c[v][w] -= d;
        f[v][w] += d;
        c[w][v] += f[w][v];
        f[w][v] = 0;
      }
      // print_matrix(c, "c");
      // print_matrix(f, "f");
      // print_vector(e, "e");
    }
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
      //c[i][s] += f[s][i];  // There can be an arc back already.
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
      //   cout<<"cut by flow\n";
      //   c[w][v] = c_in[w][v];
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

vector<vector<int>> push_relabel_proxy(
    const vector<vector<int>> &c,
    int s,
    int t) {
  return push_relabel(c, s, t);
}

void test1() {
  auto m = make_matrix(4);
  m[0][1] = 1;
  m[0][2] = 4;
  m[1][2] = 5;
  m[2][1] = 2;
  m[1][3] = 4;
  m[2][3] = 1;
  auto f = push_relabel_proxy(m, 0, m.size() - 1);
  CHECK(4 == get_flow(f[0]));
  auto f_map = flow_map(f);
  CHECK(5 == f_map.size());
  CHECK(1 == f_map[make_pair(0, 1)]);
  CHECK(3 == f_map[make_pair(0, 2)]);
  CHECK(2 == f_map[make_pair(2, 1)]);
  CHECK(1 == f_map[make_pair(2, 3)]);
  CHECK(3 == f_map[make_pair(1, 3)]);
}

void test2() {
  auto m = make_matrix(2);
  m[0][1] = 3;
  auto f = push_relabel_proxy(m, 0, m.size() - 1);
  CHECK(3 == get_flow(f[0]));
  auto f_map = flow_map(f);
  CHECK(1 == f_map.size());
  CHECK(3 == f_map[make_pair(0, 1)]);
}

void test3() {
  auto m = make_matrix(5);
  m[0][1] = 1;
  m[0][2] = 4;
  m[1][2] = 5;
  m[2][1] = 2;
  m[1][3] = 4;
  m[2][3] = 1;
  m[3][4] = 3;
  m[4][3] = 1;
  auto f = push_relabel_proxy(m, 0, m.size() - 1);
  CHECK(3 == get_flow(f[0]));
  auto f_map = flow_map(f);
  CHECK(5 == f_map.size());
  CHECK(3 == f_map[make_pair(0, 2)]);
  CHECK(2 == f_map[make_pair(2, 1)]);
  CHECK(1 == f_map[make_pair(2, 3)]);
  CHECK(3 == f_map[make_pair(3, 4)]);
  CHECK(2 == f_map[make_pair(1, 3)]);
}

void test4() {
  auto m = make_matrix(5);
  m[0][1] = 4;
  m[1][2] = 1;
  m[1][3] = 2;
  m[3][4] = 2;
  m[2][4] = 2;
  auto f = push_relabel_proxy(m, 0, m.size() - 1);
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
  test1();
  test2();
  test3();
  test4();
  cout << "TESTS PASSED." << endl;
  return 0;
}
