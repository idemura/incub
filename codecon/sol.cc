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
  int i = 1, j = 0;
  while (i < s.size()) {
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
  pf.push_back(j);
  return pf;
}

int main(int argc, char **argv) {
  ios_base::sync_with_stdio(false);
  int n, k;
  string s;
  cin >> n >> k >> s;
  cout << s << endl;
  for (auto c : s) cout << c << " ";
  cout << endl;
  const auto& pf = prefix_fn(s);
  for (int i = 1; i < pf.size(); i++) {
    cout << pf[i] << " ";
  }
  cout << endl;
  // assert(pf[4] == 2);
  string res(s.size(), 0);
  for (int i = 1; i < pf.size(); i++) {
    int prefix_part = i - pf[i];
    if (pf[i] / prefix_part + 1 == k) {
      res[i - 1] = '1';
    } else {
      res[i - 1] = '0';
    }
  }
  cout << "0 0 0 1 1 0 0 0 0 1 1 1 1 1  1  0  0  0  0  1  1" << endl;
  cout << res << endl;
  return 0;
}
