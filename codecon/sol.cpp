#include <algorithm>
#include <map>
#include <vector>
#include <math.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define INF 0x7fffffff

typedef long long int lli;

void sieve(int n, std::vector<int> *primes)
{
  char *seq = new char[n + 1]();
  // +1 for case of precise square of large magnitude. Don't sure if it's
  // actually needed.
  int sqrtn = (int)sqrt(n + 1);
  int i;
  for (i = 2; i <= sqrtn; i++) {
    if (seq[i]) {
      continue;
    }
    for (int j = i * i; j <= n; j += i) {
      seq[j] = 1;
    }
    primes->push_back(i);
  }
  for (; i <= n; i++) {
    if (!seq[i]) {
      primes->push_back(i);
    }
  }
  delete[] seq;
}

int main(int argc, char **argv)
{
// #ifndef ONLINE_JUDGE
//     freopen("in", "r", stdin);
// #endif

  std::vector<int> primes;
  sieve(42, &primes);
  return 0;
}
