#include "base.h"

constexpr int kBlack = 0;
constexpr int kWhite = 1;

// I call nodes from the i-th list 'white' and nodes cascaded from (i-1)-th
// level 'black'.
struct FC_Node {
  int v = 0;  // Value.
  int c = kBlack;  // Color.
  // Inverted color index. Closest white to the right of a black node and
  // closest to the left black node of a white node.
  int inv_ix = 0;
  int cascade_ix = 0;  // Cascaded index, for black node only.
};

struct FC {
  vector<FC_Node> nodes;
};

vector<FC_Node> fc_build_first(const vector<int> &a) {
  vector<FC_Node> res(a.size());
  for (int i = 0; i < a.size(); i++) {
    res[i].v = a[i];
    res[i].c = kWhite;
    res[i].inv_ix = -1;
    res[i].cascade_ix = -1;
  }
  return res;
}

// For white, we do 2 things:
//   1. Set @index of the previous black node,
//   2. For all black nodes before it we set @index to @wi. This step I defer
//      until the end, where I go back and set it.
vector<FC_Node> fc_build_cascade(
    const vector<int> &w,
    const vector<FC_Node> &b) {
  vector<FC_Node> res(w.size() + b.size() / 2);
  int wi = 0, bi = 0;
  int prev_black = -1;  // Previous cascaded node.
  for (int ri = 0; bi < b.size() && wi < w.size(); ri++) {
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
void fc_build(const vector<vector<int>> &lists) {
  FC fc;
  fc.vodes.resize(lists.size());
  fc.vodes[0] = fc_build_first(lists[0]);
  for (int i = 1; i < lists.size(); i++) {
    fc.vodes[i] = fc_build_cascade(lists[i], fc.vodes[i - 1]);
  }
}

void fc_search() {
}

int main(int argc, char **argv) {
  ios_base::sync_with_stdio(false);
  test();
  return 0;
}
