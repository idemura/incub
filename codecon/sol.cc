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
using u64 = unsigned long long int;
using i32 = int;
using u32 = unsigned int;

constexpr char kEol[] = "\n";
constexpr int INF = 0x7fffffff;
constexpr int MOD = 1000000007;

int main(int argc, char **argv) {
  ios_base::sync_with_stdio(false);
  int n = 0, k = 0;
  scanf("%d%d", &n, &k);
  vector<int> a(n);
  for (int i = 0; i < n; i++) {
    scanf("%d", &a[i]);
  }
  unordered_map<int, int> right_pos;
  int longest_l = 0, longest_r = 0;
  int c = 0;
  int l = 0;
  for (int i = 0; i < n; i++) {
    auto it = right_pos[a[i]];
    if (right_pos.end() == it) {
      // See a[i] first time (since @l). Increase cardinality.
      if (c == k) {
        // Pull left end to right maintaining longest possible until cardinality
        // drops by one.
        for (; l < i; l++) {
          if (right_pos[a[l]] == l) {
            right_pos.erase(a[l]);
            break;
          }
        }
      } else {
        c++;
        right_pos[a[i]] = i;
      }
    } else {
      right_pos[a[i]] = i;
    }
    if (i - l > longest_r - longest_r) {
      longest_l = l;
      longest_r = i;
    }
  }
  print("%d %d\n", longest_l + 1, longest_r + 1);
  return 0;
}

