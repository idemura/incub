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

void print_prefix_fn(const string &s, const vector<int> &pf) {
  for (int i = 0; i < s.size(); i++) {
    cout << s << "\n";
    auto p = string(i - pf[i], ' ') + s.substr(0, pf[i]) + string("^\n");
    cout << p;
  }
}

void test(const string &s) {
  auto pf = prefix_fn(s);
  print_prefix_fn(s, pf);
}

int main(int argc, char **argv) {
  ios_base::sync_with_stdio(false);
  test("ababba");
  test("aaaa");
  return 0;
}
