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
#include <assert.h>
#include <stdlib.h>
#include <string.h>

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

using Idx2i = pair<int, int>;

// Range with both ends included.
vector<Idx2i> all_palindromes(const string &s) {
  vetor<Idx2i> res;
  unordered_map<Idx2i> p;
  for (int i = 0; i < s.size(); i++) {
    p[Idx2i(i, i)] = true;
  }
  for (int i = 0; i + 1 < s.size(); i++) {
    if (s[i] == s[i + 1]) {
      p[Idx2i(i, i)] = true;
    }
  }
  for (int i = 3; i <= s.size(); i++) {
    for (auto &r : p[i & 1]) {

      up.emplace_back();
    }
  }
}

int main(int argc, char **argv) {
  ios_base::sync_with_stdio(false);
  // aaba
  // started from a[k]: a[i] i want to know if a[k+1 .. i-1] is a palindrome.
  // next step:
  // a[k+1 .. i] => need to know a[k+1 .. i-2]
  // next step:
  // a[k+1 .. i+1] => need to know a[k+1 .. i-1] to update.
  // next step:
  // a[k+1 .. i+2] => need to know a[k+1 .. i]
  // or:
  // check pals of length 1.
  // check pals of length 2.
  // get pals of length 1 and update.
  return 0;
}
