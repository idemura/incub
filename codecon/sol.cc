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

class RoadConstruction {
public:
  int getExitTime(vector<string> lanes) {
    lanes_ = move(lanes);
    yield_.resize(lanes_.size());
    ix_.resize(lanes_.size());
    while (step_rec(0) != 'D') {}
    return c_ - 1;
  }

  // A car exited from lanes >i or 0.
  char step_rec(int i) {
    if (i == lanes_.size()) return 0;
    if (ix_[i] == lanes_[i].size()) {
      return step_rec(i + 1);
    }
    if (yield_[i]) {
      return exit_lane(i);
    }
    auto s = step_rec(i + 1);
    if (s == 'D') {
      return s;
    }
    if (s == 0) {
      return exit_lane(i);
    }
    yield_[i] = 1;
    return s;
  }

  char exit_lane(int i) {
    c_++;
    yield_[i] = 0;
    return lanes_[i][ix_[i]++];
  }

  vector<string> lanes_;
  vector<int> ix_, yield_;
  int c_ = 0;
};

int main() {
  cout << NEW_UNIQUE(RoadConstruction)->getExitTime(
      { "AB",
        "CD",
        "E" }) << endl;
  cout << NEW_UNIQUE(RoadConstruction)->getExitTime(
      { "AAA",
        "A",
        "AAA",
        "A",
        "AAD",
        "A",
        "AAB" }) << endl;
  return 0;
}
