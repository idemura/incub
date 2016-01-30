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
constexpr int MOD = 1000000007;

class SpiralRoute {
public:
  vector <int> thronePosition(int width, int length) {
    int min_size = min(width, length);
    while (min_size >= 3) min_size -= 2;
    if (min_size == width) {
      if (min_size == 1)
        return {min_size / 2, min_size / 2 + length - min_size - 1};
      else
        return {min_size / 2 + 1, min_size / 2};
    } else {
      if (min_size == 1)
        return {min_size / 2 + length - min_size - 1, min_size / 2};
      else
        return {min_size / 2, min_size / 2 + 1};
    }
  }
};

int main() {
  return 0;
}
