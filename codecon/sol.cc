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
  int i = 0, j = 0, c = 0;
  while (i < s.size()) {
    if (j == needle.size()) {
      c++;
      j = 0;
    }
    if (s[i] & (1 << needle[j])) {
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
  return c;
}

void convert(string &s) {
  for (auto &c : s) {
    switch (c) {
      case 'A': c = 0; break;
      case 'T': c = 1; break;
      case 'G': c = 2; break;
      case 'C': c = 3; break;
    }
  }
}

int main(int argc, char **argv) {
  ios_base::sync_with_stdio(false);
  int s_len, t_len, k;
  cin >> s_len >> t_len >> k;
  string s, t;
  cin >> s >> t;
  convert(s);
  cout << "s: ";
  for (auto c: s) cout << "ATGC"[c];
  cout << endl;
  convert(t);
  cout << "t: ";
  for (auto c: t) cout << "ATGC"[c];
  cout << endl;
  // Collapse k-left-right loc into one char.
  int kc[4] = {};
  for (int i = 0; i < k; i++) {
    kc[s[i]]++;
  }
  string k_local(s.size(), 0);
  for (int i = 0; i < s.size(); i++) {
    cout << "at " << i << " kc: "
         << kc[0] << " " << kc[1] << " " << kc[2] << " " << kc[3] << endl;
    int l_rem = i - k - 1;
    cout << "l_rem=" << l_rem << " char " << (int)s[l_rem] << endl;
    if (l_rem >= 0) kc[s[l_rem]]--;
    int r_ins = i + k;
    cout << "r_ins=" << r_ins << " char " << (int)s[r_ins] << endl;
    if (r_ins < s.size()) kc[s[r_ins]]++;
    cout << "updated kc: "
         << kc[0] << " " << kc[1] << " " << kc[2] << " " << kc[3] << endl;
    k_local[i] = (kc[0]? 1: 0) | (kc[1]? 2: 0) | (kc[2]? 4: 0) | (kc[3]? 8: 0);
  }

  for (auto c : k_local) {
    cout << (c & 1? "A": "")
         << (c & 2? "T": "")
         << (c & 4? "G": "")
         << (c & 8? "C": "") << endl;
  }

  cout << kmp(k_local, t) << endl;
  return 0;
}
