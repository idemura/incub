#include "base.h"

struct FC_Node {
  FC_Node *ptr = nullptr;
  int n = 0;
  int f = 0;  // 1 if cascaded in from the previous layer.
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

void fc_build(const vector<vector<int>> &lists) {
  FC fc;
  fc.resize(lists.size());
  fc_build_first(lists[0], fc.nodes[0]);
  for (int i = 1; i < lists.size(); i++) {
    int k0 = 0;
    int k1 = 0;
    int kr = 0;
    // Previous cascaded node.
    FC_Node *prev_c = nullptr;
    fc.nodes[i].resize(lists[i].size() + fc.nodes[i - 1].size());
    while (k0 < fc.nodes[i - 1].size() && k1 < lists[i].size()) {
      if (lists[i][k1] < fc.nodes[i - 1][k0].n) {
        fc.nodes[i][kr].n = list[i][k];
        fc.nodes[i][kr].f = 0;
        fc.nodes[i][kr].ptr = prev_c;
        kr++;
      } else {
      }
    }

  }
}

void fc_search() {
}

int main(int argc, char **argv) {
  ios_base::sync_with_stdio(false);
  test();
  return 0;
}
