#include <algorithm>
#include <functional>
#include <list>
#include <map>
#include <set>
#include <chrono>
#include <unordered_map>
#include <memory>
#include <vector>
#include <string>
#include <random>
#include <limits>
#include <sstream>
#include <utility>
#include <iostream>
#include <cstdlib>
#include <cmath>  // Overloads for abs.

using namespace std;

using i64 = long long int;
using u64 = unsigned long long int;
using i32 = int;
using u32 = unsigned int;

constexpr char kEol[] = "\n";
constexpr int INF = 0x7fffffff;
// constexpr int MOD = 100000007;
constexpr int MOD = 19;

constexpr int mpow(i64 a, int k, int p) {
  i64 x = 1;
  for (; k; k >>= 1) {
    if (k & 1) {
      x *= a;
      x %= p;
    }
    a *= a;
    a %= p;
  }
  return (int)x;
}

constexpr int minv(int a, int p) {
  return mpow(a, p - 2, p);
}

constexpr i64 k2inv = minv(2, MOD);

i64 naive(i64 n, i64 m) {
  i64 s = 0;
  for (int i = 1; i <= m; i++) {
    s += n % i;
    cout<<n<<" % "<<i<<" = "<<(n % i)<<" q="<<(n / i)<<endl;
  }
  cout<<"naive "<<s<<" mod "<<MOD<<" "<<(s % MOD)<<endl;
  return s;
}

i64 solve(i64 n, i64 m) {
  i64 s = 0;
  if (m > n) {
    s += n * (m - n);
    cout<<"s after n "<<s<<endl;
    m = n;
  }
  i64 u = n;
  for (i64 q = 1;; q++) {
    i64 l = n / (q + 1) + 1;
    if (m < l) continue;
    if (m < u) u = m;
    i64 num_members = u - l + 1;
    cout<<"num_members="<<num_members<<endl;
    cout<<"u="<<u<<" l="<<l<<endl;
    if (num_members <= 1) break;
    cout<<"from "<<l<<" till "<<u<<" quot is "<<q<<endl;
    i64 r0 = n % u;
    i64 r1 = n % l;
    cout<<"r0="<<r0<<" r1="<<r1<<endl;
    cout<<"s="<<s<<endl;
    s += (r0 + r1) * num_members / 2;
    cout<<"s="<<s<<endl;
    u = l - 1;
  }
  cout<<"u="<<u<<endl;
  for (int i = 1; i <= u; i++) {
    cout<<"micro "<<i<<endl;
    s += n % i;
  }
  cout<<s<<endl;
  return s;
}

void full_test() {
  int c = 0;
  for (int n = 1; n <= 25; n++) {
    for (int m = 1; m <= 30; m++) {
      i64 a = naive(n, m);
      i64 b = solve(n, m);
      if (a != b) {
        cerr<<"ERROR for n="<<n<<" m="<<m<<endl;
        cerr<<"  naive: "<<a<<endl;
        cerr<<"  solve: "<<b<<endl;
      }
      c++;
    }
  }
  cerr<<"full test is done. c="<<c<<endl;
}

int main(int argc, char **argv) {
  ios_base::sync_with_stdio(false);
  //i64 n, m;
  //cin>>n>>m;
  //n = 12;
  //m = 7;
  //naive(n, m);
  //solve(n, m);
  full_test();
  return 0;
}

