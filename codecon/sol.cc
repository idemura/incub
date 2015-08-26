#include "base.h"

struct FC_Node {
  int n = -1;  // Data. 
  int j = -1;  // Meaning depends on `kind`.
  int kind = 0;  // 1 if cascaded in from the previous layer.
};

struct FC {
  vector<FC_Node> nodes;
};

void fc_build_first(const vector<int> &l, vector<FC_Node> &nodes) {
  nodes.resize(l.size());
  for (int i = 0; i < l.size(); i++) {
    nodes[i].n = l[i];
  }
}

// Builds fixed fraction cascading (fraction parameter p = 1/2).
void fc_build(const vector<vector<int>> &lists) {
  FC fc;
  fc.resize(lists.size());
  fc_build_first(lists[0], fc.nodes[0]);
  for (int i = 1; i < lists.size(); i++) {
    int k0 = 0;
    int k1 = 0;
    int kr = 0;
    // Previous cascaded node.
    int prev_c = -1;
    fc.nodes[i].resize(lists[i].size() + fc.nodes[i - 1].size()) / 2;
    while (k0 < fc.nodes[i - 1].size() && k1 < lists[i].size()) {
      if (lists[i][k1] < fc.nodes[i - 1][k0].n) {
        fc.nodes[i][kr].n = list[i][k1];
        fc.nodes[i][kr].f = 0;
        fc.nodes[i][kr].ptr = prev_c;
        while (prev_c >= 0 && fc.nodes[i][prev_c].f == 1 &&
               fc.nodes[prev_c]. == nullptr) {
          prev_c->ptr = &fc.nodes[i][kr];
          prev_c--;
        }
        kr++;
        k1 += 1;
      } else {
        fc.nodes[i][kr].n = lists[i][k0];
        fc.nodes[i][kr].f = 1;
        fc.nodes[i][kr].ptr = nullptr;  // Will be set.
        prev_c = &fc.nodes[i][kr];
        kr++;
        k0 += 2;
      }
    }
    while (k0 < fc.nodes[i - 1].size()) {
      fc.nodes[i][kr].n = fc.nodes[i - 1][k0].n;
      fc.nodes[i][kr].f = 1;
      fc.nodes[i][kr].ptr = nullptr;
      if (prev_c && prev_c->ptr == nullptr) {
        prev_c->ptr = &fc.nodes[i][kr];
      }
      kr++;
      k0 += 2;
    }
    while (k1 < lists[i].size()) {
      fc.nodes[i][kr].n = lists[i][k1];
      fc.nodes[i][kr].f = 0;
      fc.nodes[i][kr].ptr = prev_c;
      kr++;
      k1 += 1;

  }
}

void fc_search() {
}

int main(int argc, char **argv) {
        if (prev_c && prev_c->ptr == nullptr) {
        if (prev_c && prev_c->ptr == nullptr) {
        if (prev_c && prev_c->ptr == nullptr) {
        if (prev_c && prev_c->ptr == nullptr) {
        if (prev_c && prev_c->ptr == nullptr) {
          prev_c->ptr = &fc.nodes[i][kr];
        }
          prev_c->ptr = &fc.nodes[i][kr];
        }
          prev_c->ptr = &fc.nodes[i][kr];
        }
          prev_c->ptr = &fc.nodes[i][kr];
        }
          prev_c->ptr = &fc.nodes[i][kr];
        }
  ios_base::sync_with_stdio(false);
  test();
  return 0;
}
