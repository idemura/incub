#include <algorithm>
#include <functional>
#include <iostream>
#include <list>
#include <map>
#include <unordered_map>
#include <string>
#include <queue>
#include <vector>
#include <memory>
#include <sstream>
#include <utility>
#include <math.h>
#include <stdlib.h>

#define ARRAY_SIZEOF(A) (sizeof(A) / sizeof(A[0]))
#define NON_COPYABLE(C) \
    C(const C&) = delete; \
    C& operator=(const C&) = delete;

#define CHECK(E) \
  do { \
      if (!(E)) { \
        cout << "CHECK failed at " << __FILE__ << "@" << __LINE__ << endl; \
        exit(EXIT_FAILURE); \
    } \
  } while (false)

using namespace std;

using i64 = long long int;
using i32 = int;

constexpr char kEol[] = "\n";
constexpr int INF = 0x7fffffff;
constexpr int MOD = 100000007;

using ListOfList = vector<vector<int>>;

template<class T>
vector<T> append(vector<T> v, T x) {
  v.push_back(x);
  return v;
}

ListOfList get_partitions(int n, const vector<int> &sizes) {
  vector<ListOfList> t(n + 1);
  t[0].push_back({});
  for (int i = 0; i <= n; i++) {
    for (auto x : sizes) {
      int j = i + x;
      if (j > n) break;
      for (auto v : t[i]) {
        if (v.empty() || v.back() <= x) {
          t[j].push_back(append(move(v), x));
        }
      }
    }
  }
  return move(t[n]);
}

int main(int argc, char **argv)
{
  ios_base::sync_with_stdio(false);
  auto r = get_partitions(6, {1, 2});
  for (auto &v : r) {
    for (auto n : v) cout << n << " ";
    cout << endl;
  }
  return 0;
}
