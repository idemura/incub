#include "base.h"

// Result[i] means minimal divisor of i.
vector<int> sieve(int n) {
  vector<int> factor(n + 1);
  factor[1] = 1;
  for (int i = 2; i <= n; i++) {
    if (factor[i] != 0) continue;
    factor[i] = i;
    for (int j = i * i; j <= n; j += i) {
      if (factor[j] == 0) {
        factor[j] = i;
      }
    }
  }
  return move(factor);
}

vector<pair<int, int>> factorize(const vector<int> &factor, int n) {
  vector<pair<int, int>> result;
  if (n <= 1) return result;
  auto p = make_pair(factor[n], 1);
  n /= factor[n];
  while (n != 1) {
    if (p.first != factor[n]) {
      p.second++;
    } else {
      result.push_back(p);
      p = make_pair(factor[n], 1);
    }
    n /= factor[n];
  }
  result.push_back(p);
  return move(result);
}

vector<int> primes(int n) {
  vector<int> sieve(n + 1), result;
  for (int i = 2; i <= n; i++) {
    if (sieve[i] != 0) continue;
    result.push_back(i);
    for (int j = i * i; j <= n; j += i) {
      sieve[j] = 1;
    }
  }
  return move(result);
}

void test1() {
  CHECK(sieve(27) == vector<int>(
        {0, 1, 2, 3, 2, 5, 2, 7, 2, 3, 2,
         11, 2, 13, 2, 3, 2, 17, 2, 19, 2,
         3, 2, 23, 2, 5, 2, 3}));
}

void test2() {
  auto s = sieve(27);
  auto f2 = factorize(s, 2);
  CHECK(f2[0] == make_pair(2, 1));
  auto f3 = factorize(s, 3);
  CHECK(f3[0] == make_pair(3, 1));
  auto f4 = factorize(s, 4);
  CHECK(f4[0] == make_pair(2, 2));
}

int main(int argc, char **argv) {
  ios_base::sync_with_stdio(false);
  test1();
  test2();
  cout << "TESTS PASSED." << endl;
  return 0;
}
