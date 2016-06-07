#include <iostream>
#include <algorithm>
#include <vector>
#include <string>
#include <cstdio>

using namespace std;

using i64 = long long int;

int main() {
  int an = 0, qn = 0;
  scanf("%d", &an);
  vector<int> a(an);
  for (auto &x : a) scanf("%d", &x);
  scanf("%d", &qn);
  vector<int> q(qn);
  for (auto &x : q) scanf("%d", &x);
  sort(a.begin(), a.end());
  auto positive = lower_bound(a.begin(), a.end(), 0);
  auto s = 0;
  for (auto x : q) {
    s += x;
    vector<int>::iterator i;
    if (s < 0) {
      i = lower_bound(positive, a.end(), -s);
    } else {
      i = lower_bound(a.begin(), positive, -s);
    }
  }
  return 0;
}
