#include "base.h"

constexpr int kBlack = 0;
constexpr int kWhite = 1;

// I call nodes from the i-th list 'white' and nodes cascaded from (i-1)-th
// level 'black'.
struct Node {
  int v = 0;  // Value.
  int c = kBlack;  // Color.
  // Inverted color index. Closest white to the right of a black node and
  // closest to the left black node of a white node.
  int inv_ix = 0;
  int cascade_ix = 0;  // Cascaded index, for black node only.
};

struct FCascade {
  vector<vector<Node>> nodes;
};

vector<Node> build_first(const vector<int> &a) {
  vector<Node> res(a.size());
  for (int i = 0; i < a.size(); i++) {
    res[i].v = a[i];
    res[i].c = kWhite;
    res[i].inv_ix = res[i].cascade_ix = -1;
  }
  return res;
}

// For white, we do 2 things:
//   1. Set @index of the previous black node,
//   2. For all black nodes before it we set @index to @wi. This step I defer
//      until the end, where I go back and set it.
vector<Node> build_cascade(
    const vector<int> &w,
    const vector<Node> &b) {
  vector<Node> res(w.size() + b.size() / 2);
  int wi = 0, bi = 0, ri = 0;
  int prev_black = -1;  // Previous cascaded node.
  for (; bi < b.size() && wi < w.size(); ri++) {
    if (w[wi] < b[bi].v) {
      res[ri].v = w[wi];
      res[ri].c = kWhite;
      res[ri].inv_ix = prev_black;
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
    res[ri].v = b[bi].v;
    res[ri].c = kBlack;
    res[ri].cascade_ix = bi;
  }
  for (; wi < w.size(); wi += 1, ri++) {
    res[ri].v = w[wi];
    res[ri].c = kWhite;
    res[ri].inv_ix = prev_black;
  }
  int prev_white = -1;
  for (int i = res.size(); i-- > 0; ) {
    if (res[i].c == kWhite) {
      prev_white = i;
    } else {
      res[i].inv_ix = prev_white;
    }
  }
  return res;
}

// Builds fixed fraction cascading (fraction parameter p = 1/2).
void build(const vector<vector<int>> &lists) {
  FCascade fc;
  fc.nodes.resize(lists.size());
  fc.nodes[0] = build_first(lists[0]);
  for (int i = 1; i < lists.size(); i++) {
    fc.nodes[i] = build_cascade(lists[i], fc.nodes[i - 1]);
  }
}

// Returns vector of list index (first) and index in the list (second) where
// `n` where found.
vector<pair<int, int>> search(const FCascade &fc, int n) {
  vector<pair<int, int>> loc;
  auto it = lower_bound(fc.nodes[0].begin(), fc.nodes[0].end(), n,
        [](const Node& node, int n) {
          return node.v < n;
        });
  int i0 = it == fc.nodes[0].end()
      ? fc.nodes[0].size()
      : it - fc.nodes[0].begin();
  cout<<"i0="<<i0<<endl;
  return loc;
}

void test() {
}

int main(int argc, char **argv) {
  ios_base::sync_with_stdio(false);
  test();
  return 0;
}
