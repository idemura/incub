#include "base.h"

using VertexVec = vector<int>;

// A good explanation at Wikipedia:
// http://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm
class SccTarjan {
public:
  static vector<VertexVec> run(const vector<VertexVec> &a) {
    SccTarjan t(a);
    return t.traverse();
  }

private:
  explicit SccTarjan(const vector<VertexVec> &a) : a_(a) {
    i_.resize(a_.size(), -1);
    r_.resize(a_.size(), -1);
    on_stack_.resize(a_.size());
  }

  vector<VertexVec> traverse() {
    for (int i = 0; i < a_.size(); i++) {
      if (i_[i] < 0) dfs(i);
    }
    return move(scc_);
  }

  void dfs(int v) {
    i_[v] = vertex_counter_++;
    r_[v] = i_[v];
    on_stack_[v] = 1;
    s_.push_back(v);

    for (auto w : a_[v]) {
      if (i_[w] < 0) {
        dfs(w);
        r_[v] = min(r_[v], r_[w]);
      } else if (on_stack_[w]) {
        r_[v] = min(r_[v], w);
      }
    }

    if (r_[v] == i_[v]) {
      VertexVec scc;
      for (int w = -1; w != v;) {
        w = s_.back();
        s_.pop_back();
        on_stack_[w] = 0;
        scc.push_back(w);
      }
      scc_.push_back(move(scc));
    }
  }

  const vector<VertexVec> &a_;
  int vertex_counter_ = 0;
  vector<int> i_;
  vector<int> on_stack_;
  vector<int> s_;  // Stack.
  vector<int> r_;  // Root index (low link).
  vector<VertexVec> scc_;
  NON_COPYABLE(SccTarjan);
};

vector<VertexVec> scc_tarjan(const vector<VertexVec> &a) {
  return SccTarjan::run(a);
}

void normalize_scc(vector<VertexVec> &scc) {
  for (auto &g : scc) {
    sort(g.begin(), g.end());
  }
  auto cmp_vector_first = [](const VertexVec &a, const VertexVec &b) {
    return a[0] < b[0];
  };
  sort(scc.begin(), scc.end(), cmp_vector_first);
}

template<class T>
bool vector_eq(const vector<T> &a, const vector<T> &b) {
  return a.size() == b.size() && equal(a.begin(), a.end(), b.begin());
}

void test1() {
  auto scc = scc_tarjan({
      {1},
      {2},
      {0},
      {0, 2}});
  normalize_scc(scc);
  CHECK(scc.size() == 2);
  CHECK(vector_eq(scc[0], {0, 1, 2}));
  CHECK(vector_eq(scc[1], {3}));
}

void test2() {
  auto scc = scc_tarjan({
      {1, 3},
      {2},
      {1},
      {4},
      {5},
      {3}});
  normalize_scc(scc);
  CHECK(scc.size() == 3);
  CHECK(vector_eq(scc[0], {0}));
  CHECK(vector_eq(scc[1], {1, 2}));
  CHECK(vector_eq(scc[2], {3, 4, 5}));
}

void test3() {
  auto scc = scc_tarjan({
      {1, 2},
      {2},
      {3},
      {0}});
  normalize_scc(scc);
  CHECK(scc.size() == 1);
  CHECK(vector_eq(scc[0], {0, 1, 2, 3}));
}

int main() {
  ios_base::sync_with_stdio(false);
  test1();
  test2();
  test3();
  cout << "TESTS PASSED." << endl;
  return 0;
}
