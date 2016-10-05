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
    const vector<int> &c) {
  cout<<"get_vertex_downhill v="<<v<<"\n";
  print_vector(h, "h");
  print_vector(c, "c");
  const auto n = c.size();
  for (int i = 0; i < n; i++) {
    if (h[i] < h[v] - 1 && c[i] > 0) {
      return i;
    }
  }
  return -1;
}

void push_relabel_correct(vector<vector<int>> &c, int s, int t) {
  cout<<"s="<<s<<" t="<<t<<endl;
  print_matrix(c, "c");
  const auto n = c.size();
  vector<vector<int>> f(n);
  for (auto &r : f) r.resize(n);
  vector<int> h(n);
  h[s] = n;
  vector<int> e(n);
  // Full flow on edges from @s.
  for (int i = 0; i < n; i++) {
    if (c[s][i] > 0) {
      e[i] = f[s][i] = c[s][i];
      c[i][s] += f[s][i];  // There can be an arc back already.
      c[s][i] = 0;
    }
  }

  for (;;) {
    int v = get_vertex(h, e, s, t);
    if (v < 0) {
      cout<<"flow done."<<endl;
      break;
    }
    cout<<"pick vertex v="<<v<<endl;
    int w = get_vertex_downhill(v, h, c[v]);
    cout<<"downhill vertex w="<<w<<endl;
    if (w >= 0) {
      // Push
      auto delta = min(c[v][w], e[v]);
      f[v][w] += delta;
      c[v][w] -= delta;
      c[w][v] += delta;
      e[v] -= delta;
      cout<<"push\n";
      print_matrix(c, "c");
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
