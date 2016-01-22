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
constexpr int MOD = 100000007;

void naive(i64 n, i64 m) {
  i64 s = 0;
  for (int i = 1; i <= m; i++) {
    s += n % i;
    cout<<n<<" % "<<i<<" = "<<(n % i)<<" q="<<(n / i)<<endl;
  }
  cout<<"naive "<<s<<endl;
}

void solve(i64 n, i64 m) {
  i64 s = 0;
  if (m > n) {
    s += (n + 1 + m) * (m - n) / 2;
    m = n;
  }
  i64 u = m;
  for (i64 q = 1;; q++) {
    i64 l = n / (q + 1) + 1;
    cout<<"u="<<u<<" l="<<l<<endl;
    if (u - l <= 0) break;
    cout<<"from "<<l<<" till "<<u<<" quot is "<<q<<endl;
    s += (u + l) * (u - l + 1) / 2;
    u = l - 1;
  }
  cout<<"u="<<u<<endl;
  for (int i = 1; i <= u; i++) {
    s += n % i;
  }
  cout<<s<<endl;
}


int main(int argc, char **argv) {
  ios_base::sync_with_stdio(false);
  i64 n, m;
  cin>>n>>m;
  naive(n, m);
  solve(n, m);
  return 0;
}

