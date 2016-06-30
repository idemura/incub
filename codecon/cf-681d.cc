#include <algorithm>
#include <functional>
#include <list>
#include <map>
#include <memory>
#include <random>
#include <string>
#include <unordered_map>
#include <vector>
#include <sstream>
#include <utility>
#include <iostream>
#include <cmath>  // Overloads for abs.

using namespace std;

using i64 = long long int;
using u64 = unsigned i64;
using i32 = int;
using u32 = unsigned i32;

constexpr int INF = 0x7fffffff;
constexpr int MOD = 1000000007;

struct Node {
  int children = 0;
  int to = -1, from = -1;
  Node *p = nullptr;

  Node() = default;
};

int main(int argc, char **argv) {
  int n, m, p, q, a;
  cin>>n>>m;
  vector<Node> nodes(n);
  for (int i = 0; i < n; i++) {
    nodes[i].n = i;
  }
  for (int i = 0; i < m; i++) {
    cin>>p>>q;
    nodes[q - 1].parent = &nodes[p - 1];
    nodes[p - 1].children++;
  }
  for (int i = 0; i < n; i++) {
    cin>>a;
    nodes[i].to = a - 1;
    nodes[a - 1].from = i;
  }
  vector<Node*> leaf;
  for (auto &n : nodes) {
    if (n.children == 0) leaf.push_back(&n);
  }
  for (int i = 0; i < leaf; i++) {
  }
  return 0;
}

