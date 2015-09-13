#include "base.h"

template<class T>
class FCascade {
public:
  using ValueT = T;

  FCascade() {}
  DEFAULT_COPY(FCascade);

  void clear() { cascades_.clear(); }
  void add_list(const vector<ValueT> &a);
  vector<pair<int, int>> search(ValueT x) const;
  void print() const;

private:
  static constexpr int kBlack = 0;
  static constexpr int kWhite = 1;

  // I call nodes from the i-th list 'white' and nodes cascaded from (i-1)-th
  // level 'black'.
  struct Node {
    ValueT v = 0;  // Value.
    int c = kBlack;  // Color.
    // If c == kBlack: next white to the right, OR
    // If c == kWhite: next black to the left.
    int j = 0;
    int cascade_ix = 0;  // Cascaded index, for black node only.
  };

  static void build_cascade(const vector<ValueT> &w, const vector<Node> &b,
                            vector<Node> &res);

  list<vector<Node>> cascades_;
};

// For white, we do 2 things:
//   1. Set @index of the previous black node,
//   2. For all black nodes before it we set @index to @wi. This step I defer
//      until the end, where I go back and set it.
template<class T>
// static
void FCascade<T>::build_cascade(const vector<T> &w, const vector<Node> &b,
      vector<Node> &res) {
  cout<<"w.size="<<w.size()<<" b.size="<<b.size()<<endl;
  res.resize(w.size() + (b.size() + 1) / 2);
  int wi = 0, bi = 0, ri = 0;
  int prev_black = -1;  // Previous cascaded node.
  for (; bi < b.size() && wi < w.size(); ri++) {
    CHECK(ri < res.size());
    if (w[wi] < b[bi].v) {
      res[ri].v = w[wi];
      res[ri].c = kWhite;
      res[ri].j = prev_black;
      wi += 1;
    } else {
      res[ri].v = b[bi].v;
      res[ri].c = kBlack;
      res[ri].cascade_ix = bi;
      prev_black = ri;
      bi += 2;
    }
  }
  for (; bi < b.size(); bi += 2, ri++) {
    CHECK(ri < res.size());
    res[ri].v = b[bi].v;
    res[ri].c = kBlack;
    res[ri].cascade_ix = bi;
  }
  for (; wi < w.size(); wi += 1, ri++) {
    CHECK(ri < res.size());
    res[ri].v = w[wi];
    res[ri].c = kWhite;
    res[ri].j = prev_black;
  }
  CHECK(res.size() == ri);
  int prev_white = -1;
  for (int i = res.size(); i-- > 0; ) {
    if (res[i].c == kWhite) {
      prev_white = i;
    } else {
      res[i].j = prev_white;
    }
  }
}

// Builds fixed fraction cascading (fraction parameter p = 1/2).
template<class T>
void FCascade<T>::add_list(const vector<T> &a) {
  if (cascades_.empty()) {
    cascades_.emplace_back(a.size());
    auto &nl = cascades_.back();
    for (int i = 0; i < a.size(); i++) {
      nl[i].v = a[i];
      nl[i].c = kWhite;
      nl[i].j = nl[i].cascade_ix = -1;
    }
  } else {
    auto &nl_prev = cascades_.back();
    cascades_.emplace_back();
    build_cascade(a, nl_prev, cascades_.back());
  }
}

// Returns vector of list index (first) and index in the list (second) where
// `n` where found.
template<class T>
vector<pair<int, int>> FCascade<T>::search(T x) const {
  vector<pair<int, int>> loc;
  if (cascades_.empty()) return loc;

  // int c = cascades_.size() - 1;
  // auto it = lower_bound(cascades_[c].begin(), cascades_[c].end(), n,
  //     [] (const Node& node, int n) {
  //       return node.v < n;
  //     });
  // int i0 = it == cascades_[c].end()
  //     ? cascades_[c].size()
  //     : it - cascades_[c].begin();
  // cout<<"i0="<<i0<<endl;
  // if (cascades_.
  return loc;
}

template<class T>
void FCascade<T>::print() const {
  int l = 0;
  for (auto &nl : cascades_) {
    cout<<"----cascades_: level="<<(l++)<<endl;
    for (int i = 0; i < nl.size(); i++) {
      cout<<"Node #"<<i
          <<"\n  color="<<(nl[i].c == kWhite ? "white" : "black")
          <<"\n  v="<<nl[i].v
          <<"\n  j="<<nl[i].j
          <<"\n  cascade_ix="<<nl[i].cascade_ix<<endl;
    }
  }
}

void test() {
  FCascade<int> fc;
  fc.add_list({1, 10, 20});
  fc.add_list({5, 9, 15, 24});
  fc.print();
}

int main(int argc, char **argv) {
  ios_base::sync_with_stdio(false);
  test();
  return 0;
}
