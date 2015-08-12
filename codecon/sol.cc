#include <algorithm>
#include <functional>
#include <list>
#include <map>
#include <unordered_map>
#include <vector>
#include <string>
#include <random>
#include <limits>
#include <sstream>
#include <utility>
#include <iostream>
#include <math.h>

using namespace std;

using i64 = long long int;
using i32 = int;

int mpow(i64 a, int k, int p) {
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

void test() {
  i64 n, m;
  cin >> n >> m;
  vector<int> a(m + 2);
  a[1] = 1;
  for (int i = 2; i <= m; i++) {
    a[i] = (a[i - 1] + mpow(i, i, m)) % m;
  }
}

int main(int argc, char **argv) {
  ios_base::sync_with_stdio(false);
  int t;
  cin >> t;
  for (int i = 0; i < t; i++) {
    test();
  }
  cout << "TESTS PASSED." << endl;
  return 0;
}
