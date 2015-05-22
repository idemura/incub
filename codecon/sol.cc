#include <algorithm>
#include <functional>
#include <iostream>
#include <list>
#include <map>
#include <unordered_map>
#include <unordered_set>
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
#define NEW_UNIQUE(T) unique_ptr<T>(new T)
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

// Movable.
struct SetAndTotal {
  set<i64> subsums;
  i64 total = 0;
};

SetAndTotal rec_step(const vector<int> &a, i64 m, int l, int r, i64* pmax) {
  SetAndTotal st;
  if (l - r == 1) {
    st.total = a[l];
    st.subsums.insert(a[l]);
  } else {
    auto m = l + (r - l) / 2;
    auto st_l = rec_step(a, l, m, pmax);
    auto st_r = rec_step(a, m, r, pmax);
  }
  return st;
}

i64 max_subarray_mod(const vector<int> &a, i64 m) {
  i64 pmax = 0;
}

int main() {
  ios_base::sync_with_stdio(false);
  int t = 0; cin >> t;
  while (t-- > 0) {
    i64 n, m;
    cin >> n >> m;
    vector<int> a;
    for (int i = 0; i < n; i++)
      cin >> a[i];
    cout << max_subarray_mod(a, m) << endl;
  }
  return 0;
}
