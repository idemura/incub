#include <algorithm>
#include <functional>
#include <iostream>
#include <list>
#include <map>
#include <string>
#include <queue>
#include <vector>
#include <memory>
#include <sstream>
#include <math.h>
#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define ARRAY_SIZEOF(A) (sizeof(A) / sizeof(A[0]))
#define NON_COPYABLE(C) \
    C(const C&); \
    C& operator=(const C&);

using namespace std;

typedef long long int i64;

constexpr int INF = 0x7fffffff;
constexpr int DIM = 200000;

vector<int> prefix_fn(const string &s) {
  vector<int> pf(s.size());
  // j is length of matching prefix up to (not including) char i.
  for (int i = 1, j = 0; i < s.size(); ) {
    if (s[i] == s[j]) {
      pf[i] = j;
      j++;
      i++;
    } else {
      if (!pf[i]) pf[i] = j; // Set one(first) time.
      if (j == 0) {
        i++;
      } else {
        j = pf[j];
      }
    }
  }
  return pf;
}

int kmp(const string &s, const string &needle) {
  auto pf = prefix_fn(needle);
  int i = 0, j = 0;
  while (i < s.size() && j < needle.size()) {
    if (s[i] == needle[j]) {
      i++;
      j++;
    } else {
      if (j == 0) {
        i++;
      } else {
        j = pf[j];
      }
    }
  }
  return j == needle.size()? i - needle.size(): (int)string::npos;
}

void test(const string &s, const string &needle) {
  int x = kmp(s, needle);
  int y = s.find(needle);
  assert(x == y);
  if (x != y) {
    cout << "FAILED:\ns=" << s << "\nneedle=" << needle << "\n";
    cout << "  KMP " << x << " find " << y << endl;
  }
}

int main(int argc, char **argv) {
  ios_base::sync_with_stdio(false);
  test("abc abcdab abcdabcdabde", "abcdabd");
  test("abcabcabaad", "aad");
  test("abcabcabaad", "abc");
  test("abcabababcd", "ababc");
  test("abcabcabaad", "cab");
  test("abcabcabaad", "bcab");
  test("abcabcabaad", "aad");
  test("abcabcabaad", "add");
  return 0;
}
