#include <iostream>
#include <string>
#include <vector>

using namespace std;

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
    for (int i = 0; i < h.size(); i++) {
      if (e[i] > 0 && i != s && i != t) {
        return i;
      }
    }
    return -1;
}

int get_vertex_downhill(
    int v,
    const vector<int> &h,
    const vector<int> &c,
    const vector<int> &b) {
  cout<<"get_vertex_downhill from v="<<v<<"\n";
  print_vector(h, "h");
  print_vector(c, "c");
  print_vector(b, "b");
  for (int i = 0; i < h.size(); i++) {
    if (h[i] < h[v] && c[i] + b[i] > 0) {
      return i;
    }
  }
  return -1;
}

void make_matrix(vector<vector<int>> &m) {
  for (auto &r : m) r.resize(m.size());
}

void push_relabel_correct(const vector<vector<int>> &c_in, int s, int t) {
  auto c = c_in;
  cout<<"s="<<s<<" t="<<t<<endl;
  print_matrix(c, "c");
  const auto n = c.size();
  vector<vector<int>> f(n);
  make_matrix(f);
  // We can undo flow excess back where it came from.
  vector<vector<int>> b(n);
  make_matrix(b);
  vector<int> h(n);
  h[s] = n;
  vector<int> e(n);
  cout<<"__ 1a"<<endl;
  // Full flow on edges from @s.
  for (int i = 0; i < n; i++) {
    if (c[s][i] > 0) {
      e[i] = f[s][i] = c[s][i];
      //c[i][s] += f[s][i];  // There can be an arc back already.
      b[i][s] += f[s][i];
      c[s][i] = 0;
    }
  }

  // cout<<"__ 1"<<endl;
  for (;;) {
    string s1;
    getline(cin, s1);
    cout<<endl;
    // cout<<"__ 2"<<endl;
    int v = get_vertex(h, e, s, t);
    if (v < 0) {
      cout<<"flow done"<<endl;
      break;
    }
    cout<<"pick vertex v="<<v<<endl;
    int w = get_vertex_downhill(v, h, c[v], b[v]);
    cout<<"downhill to w="<<w<<endl;
    if (w >= 0) {
      // Push
      auto d = min(c[v][w] + b[v][w], e[v]);
      cout<<"push "<<d<<"\n";
      cout<<"c: "<<c[v][w]<<" b:"<<b[v][w]<<endl;
      f[v][w] += d;
      if (d > b[v][w]) {
        d -= b[v][w];
        b[v][w] = 0;
      } else {
        b[v][w] -= d;
        d = 0;
      }
      c[v][w] -= d;
      b[w][v] += d;
      // if (c[w][v] > c_in[w][v]) {
      //   cout<<"cut by flow\n";
      //   c[w][v] = c_in[w][v];
      // }
      e[v] -= d;
      e[w] += d;
      print_matrix(c, "c");
      print_matrix(c, "b");
      print_matrix(f, "f");
      print_vector(e, "e");
    } else {
      // Relabel
      cout<<"relabel\n";
      h[v]++;
      print_vector(h, "h");
    }
  }
  print_matrix(f, "final flow");
  auto full_flow = 0;
  for (int i = 0; i < n; i++) {
    full_flow += f[i][t];
  }
  cout<<"full_flow="<<full_flow<<endl;
}

void test1() {
  vector<vector<int>> m(4);
  for (auto &r : m) r.resize(m.size());
  m[0][1] = 1;
  m[0][2] = 4;
  m[1][2] = 5;
  m[2][1] = 2;
  m[1][3] = 4;
  m[2][3] = 1;
  push_relabel_correct(m, 0, 3);
}

int main() {
  test1();
  return 0;
}
