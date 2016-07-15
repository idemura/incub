#include <bits/stdc++.h>

using namespace std;

using AdjList = vector<vector<int>>;

struct Node {
  Node *up = nullptr;
};

struct Vert {
  Node *node;
};

struct Edge {
  int w = 0;
  Node *a = nullptr, *b = nullptr;

  Edge() {}
  Edge(Node *a, Node *b, int w): a(a), b(b), w(w) {}
};

// Given a set of edges, make minimally weighted MST out of it. We allow edges
// with ends in vertices of the same color.
void weighted_mst_color(vector<Edge> es) {
  sort(es.begin(), es.end(), [](const Edge &x, const Edge &y) {
    return x.w > y.w;
  });

  for (auto &e : es) {
    if (e->a == nullptr && e->b == nullptr) {
    }
  }
}

int main() {
  vector<Node> nodes;
  vector<Edge> e;
  e.emplace_back();
  weighted_mst_color(move(e));
  return 0;
}

