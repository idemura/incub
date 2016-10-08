#include "base.h"

using namespace std;

bool FLAG_step_mode = false;
/*
Seems like relabel happens until vertex has no excess or all edges saturated,
because it only goes up.
*/

// @c is matrix of capacities.
void push_relabel(vector<vector<int>> &c, int s, int t) {
/*
  const auto n = c.size();
  vector<vector<int>> f(n);
  for (auto &r : f) r.resize(n);
  vector<int> h(n);
  h[s] = n;
  vector<int> q(2 * n);
  int qmax = 0;
  // Full flow on edges from @s.
  for (int i = 0; i < n; i++) {
    if (c[s][i] > 0) {
      f[i][s] = c[i][s] = c[s][i];
      c[s][i] = 0;
      if (i != t) {
        q[h[i]].push_back(i);
      }
    }
  }

  while (true) {
  }
*/
}


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

vector<vector<int>> make_matrix(int n) {
  vector<vector<int>> m(n);
  for (auto &r : m) r.resize(m.size());
  return m;
}

void wait_enter() {
  if (FLAG_step_mode) {
    string s;
    getline(cin, s);
  }
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

void test1() {
  auto m = make_matrix(4);
  m[0][1] = 1;
  m[0][2] = 4;
  m[1][2] = 5;
  m[2][1] = 2;
  m[1][3] = 4;
  m[2][3] = 1;
  auto f = push_relabel_correct(m, 0, m.size() - 1);
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
  auto f = push_relabel_correct(m, 0, m.size() - 1);
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
  auto f = push_relabel_correct(m, 0, m.size() - 1);
  CHECK(3 == get_flow(f[0]));
  auto f_map = flow_map(f);
  CHECK(5 == f_map.size());
  CHECK(3 == f_map[make_pair(0, 2)]);
  CHECK(2 == f_map[make_pair(2, 1)]);
  CHECK(1 == f_map[make_pair(2, 3)]);
  CHECK(3 == f_map[make_pair(3, 4)]);
  CHECK(2 == f_map[make_pair(1, 3)]);
}

int main() {
  FLAG_step_mode = false;
  test1();
  test2();
  test3();
  cout << "TESTS PASSED." << endl;
  return 0;
}

