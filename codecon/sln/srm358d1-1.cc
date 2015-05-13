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

class BrokenButtons {
public:
  int minPresses(int page, vector<int> broken) {
    if (broken.size() == 10) return abs(page - 100);
    bool is_b[10] = {};
    for (auto b : broken) is_b[b] = true;
    auto min_press = abs(page - 100);
    if (!is_b[0]) {
      min_press = min(min_press, page + 1);
    }
    for (int i = 1; i < 500000; i++) {
      auto found_broken = false;
      auto n = 0;
      for (int k = i; k != 0; k /= 10) {
        n++;
        if (is_b[k % 10]) {
          found_broken = true;
          break;
        }
      }
      if (found_broken) continue;
      min_press = min(abs(i - page) + n, min_press);
    }
    return min_press;
  }
};

int main() {
  cout << NEW_UNIQUE(BrokenButtons)->minPresses(5457, {6, 7, 8}) << endl;
  cout << NEW_UNIQUE(BrokenButtons)->minPresses(80000, {8, 9}) << endl;
  cout << NEW_UNIQUE(BrokenButtons)->minPresses(0, {8, 9}) << endl;
  return 0;
}