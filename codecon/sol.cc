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

    vector<int> dig;
    for (int k = page; k != 0; k /= 10) {
      dig.push_back(k % 10);
    }
    if (dig.empty()) {
      dig.push_back(0);
    }
    if (broken.empty()) {
      return dig.size();
    }

    int broken_flag[10] = {};
    for (auto b : broken) broken_flag[b] = 1;
    for (int i = 0; i < 10; i++) {
      if (!broken_flag[i]) good.push_back(i);
    }
    vector<int> next(dig.size());
    int carry = 0;
    for (int i = 0; i < dig.size(); i++) {

    }
  }

  vector<int> good;
};

int main() {
  cout << NEW_UNIQUE(BrokenButtons)->minPresses({5, 4, 1, 8}) << endl;
  cout << NEW_UNIQUE(BrokenButtons)->minPresses({2, 3, 8, 9}) << endl;
  return 0;
}
