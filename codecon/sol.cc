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
    // cout << "i=" << i << " j=" << j << endl;
    if (s[i] == s[j]) {
      j++;
      i++;
      pf[i] = j;
      // cout << "eq, move i and j pf is " << pf[i] << endl;
    } else {
      // cout << "not equal\n";
      if (!pf[i]) {
        pf[i] = j; // Set one(first) time.
        // cout << "pf[i] is 0, set pf to " << pf[i] << endl;
      }
      if (j == 0) {
        // cout << "j is 0, move i\n";
        i++;
      } else {
        j = pf[j];
        // cout << "try new prefix of size " << j << endl;
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
  //s.resize(8);
  cout << s << endl;
  // cout << "  ";
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
    cout<<"i="<<i<<" pf="<<pf[i]<<endl;
    if (pf[i] == 0) {
      res[i - 1] = '0';
      continue;
    }
    int repeated_len = i - pf[i];
    int parts = i / repeated_len;
    cout<<"repeated_len="<<repeated_len<<" parts="<<parts<<endl;
    // if (parts < 2) {
    //   res[i-1]='0';
    //   continue;
    // }
    if (i % repeated_len == 0) {
      res[i - 1] = 'x';
      // res[i - 1] = '1';
    } else {
      int num_a = parts + 1;
      int num_b = parts;
      // if (parts == 1) {
      //   res[i - 1] = '0';
      //   continue;
      // }
      // int non_rem = i - (k + 1) * (i % repeated_len);
      // cout << "non_rem=" << non_rem << endl;
      // if (non_rem % k == 0)
      //   res[i - 1] = '1';
      // else
      //   res[i - 1] = '0';
    }
    // cout<<"full_parts="<<full_parts<<endl;
    // if (full_parts % k == 0 ||
    //     (full_parts % (k + 1) == 0 && pf[i] % repeated_len == 0)) {
    // } else {
    // }
  }
  //cout << "0 0 0 1 1 0 0 0 0 1 1 1 1 1  1  0  0  0  0  1  1" << endl;
  cout << res << endl;
  // cout << "check\n";
  // cout << "000110000111111000011\n";
  cout << "00000111100111111" << endl;
  return 0;
}
