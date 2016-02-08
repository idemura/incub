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

struct Edge {
  int a = -1, b = -1;
  Edge() = default;
  Edge(int a, int b): a(a), b(b) {}
};

int main(int argc, char **argv) {
  int n, x, y;
  scanf("%d%d%d", &n, &x, &y);
  vector<Edge> e;
  vector<int> deg(n);
  for (int i = 0; i < n-1; i++) {
    int a, b;
    scanf("%d%d", &a, &b);
    deg[a - 1]++;
    deg[b - 1]++;
    e.emplace_back(a - 1, b - 1);
  }
  i64 c = 0;
  if (x >= y) {
    auto fan_tree = false;
    for (auto d : deg) {
      if (d == n - 1) {
        fan_tree = true;
        break;
      }
    }
    c = i64(y) * (n - 2);
    if (fan_tree)
      c += x;
    else
      c += y;
  } else {
    i64 cheap_edges = i64(n - 1);
    for (auto d : deg) {
      if (d > 2) cheap_edges -= d - 2;
    }
    c = i64(x) * cheap_edges + (n - 1 - cheap_edges) * i64(y);
  }
  cout<<c<<endl;
  return 0;
}

