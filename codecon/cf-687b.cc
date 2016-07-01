#include <bits/stdc++.h>

using namespace std;

using i64 = long long int;
using u64 = unsigned i64;
using i32 = int;
using u32 = unsigned i32;

constexpr int INF = 0x7fffffff;
constexpr int MOD = 1000000007;

// From Chinese remainder theorem, we need b = c[1]*...*c[k-1]*c[k+1]*...*c[n]
// to be coprime with c[k] to be able to invert b. This means that c[1]...c[n]
// are have to be coprime.
// Additionally, k from the input that we should be less than product of all
// the c[k].

bool prod_gt(const vector<int> &a, int k) {
  i64 p = 1;
  for (auto m : a) {
    p *= m;
    if (p >= k) return false;
  }
  cout<<"prod true"<<endl;
  return true;
}

int sqrt_int(int n) {
  return int(sqrt(n));
}

// Result[i] means minimal divisor of i. Use it in `factorize`.
vector<int> sieve(int n) {
  vector<int> factor(n + 1);
  factor[1] = 1;
  auto i = 2, imax = sqrt_int(n);
  for (; i <= imax; i++) {
    if (factor[i] != 0) continue;
    factor[i] = i;
    for (int j = i * i; j <= n; j += i) {
      if (factor[j] == 0) {
        factor[j] = i;
      }
    }
  }
  for (; i <= n; i++) {
    if (factor[i] == 0) factor[i] = i;
  }
  return factor;
}

bool coprime(const vector<int> &a, int a_max) {
  auto s = sieve(a_max);
  // Factoring any number of a[i] should not intersect.
  vector<int> factor_used(a_max);
  for (auto k : a) {
    cout<<"factor "<<k<<endl;
    while (k != 1) {
      cout<<"k "<<k<<endl;
      cout<<"divisor "<<s[k]<<endl;
      if (factor_used[s[k]]) return false;
      factor_used[s[k]] = 1;
      k /= s[k];
    }
  }
  cout<<"coprime true"<<endl;
  return true;
}

int main() {
  int n, k;
  cin>>n>>k;
  vector<int> c(n);
  for (auto &m : c) cin>>m;
  if (coprime(c, 1'000'004) && prod_gt(c, k)) {
    cout<<"Yes\n";
  } else {
    cout<<"No\n";
  }
  return 0;
}

