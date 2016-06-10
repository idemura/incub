#include <iostream>
#include <algorithm>
#include <vector>
#include <string>
#include <cstdio>
#include <map>

using namespace std;

using i64 = long long int;

struct Sum {
  int s = 0, n = 0;
  Sum() {}
  Sum(int s, int n): s(s), n(n) {}
};

Sum operator-(Sum a, Sum b) {
  a.s -= b.s;
  a.n -= b.n;
  return a;
}
Sum operator+(Sum a, Sum b) {
  a.s += b.s;
  a.n += b.n;
  return a;
}

struct SumMap {
  map<int, Sum> m;
  Sum total;
};

SumMap sums(vector<int> &a) {
  SumMap sm;
  sort(a.begin(), a.end());
  int s = 0;
  for (int i = 0; i < a.size(); i++) {
    sm.m[a[i]] = Sum(s, i);
    s += a[i];
  }
  sm.total = Sum(s, a.size());
  return sm;
}

// os - opposite sign
// ss - same sign
int query(const SumMap &os, const SumMap &ss, int q) {
  auto r = ss.total.s + ss.total.n * q;
  auto lb = os.m.lower_bound(q);
  if (lb == os.m.end()) {
    r += q * os.total.n - os.total.s;
  } else {
    auto sum = lb->second;
    r += q * sum.n - sum.s;  // 0 to lower bound
    sum = os.total - sum;
    r += sum.s - q * sum.n;  // lower bound till the end
  }
  return r;
}

int main() {
  int an = 0, qn = 0;
  scanf("%d", &an);
  vector<int> neg, pos;
  for (int i = 0; i < an; i++) {
    int k = 0;
    scanf("%d", &k);
    if (k < 0)
      neg.push_back(-k);
    else
      pos.push_back(k);
  }
  auto nsm = sums(neg);
  auto psm = sums(pos);
  scanf("%d", &qn);
  int acc = 0;
  for (int i = 0; i < qn; i++) {
    int q = 0;
    scanf("%d", &q);
    acc += q;
    if (acc < 0) {
      printf("%d\n", query(nsm, psm, -acc));
    } else {
      printf("%d\n", query(psm, nsm, acc));
    }
  }
  return 0;
}
