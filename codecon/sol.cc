#include <algorithm>
#include <map>
#include <string>
#include <vector>
#include <assert.h>
#include <ctype.h>
#include <limits.h>
#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define ARRAY_SIZEOF(A) (sizeof(A) / sizeof(A[0]))
#define INF 0x7fffffff
#define NON_COPYABLE(C) \
    C(const C&); \
    C& operator=(const C&);

using namespace std;

typedef long long int lli;

struct IntMask {
  int mask, n;
  IntMask(): mask(), n() {}
};

struct IntMaskLess {
  bool operator()(const IntMask &lh, const IntMask &rh) const {
    return lh.mask < rh.mask;
  }
};

vector<int> primes;
vector<vector<int> > g;  // Adjacency list of our graph.

void sieve(int n, std::vector<int> *primes)
{
  char *seq = new char[n + 1]();
  int sqrtn = (int)sqrt(n);
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

int getTripleOfInt(int n, int triple_i) {
  switch (triple_i) {
  case 0: return n / 10;  // 123
  case 1: return (n / 100) * 10 + n % 10;  // 124
  case 2: return (n / 1000) * 100 + n % 100;  // 134
  case 3: return n % 1000;  // 234
  }
  assert(false);
  return -1;
}

int findMinimalCost(int s, int d) {
  if (s == d) {
    return 0;
  }
  return -1;
}

int main(int argc, char **argv) {
#ifndef ONLINE_JUDGE
  freopen("in", "r", stdin);
#endif

  sieve(10000, &primes);
  primes.erase(primes.begin(), lower_bound(primes.begin(), primes.end(), 1000));

  vector<IntMask> masked(primes.size());
  g.resize(primes.size());
  for (int t = 0; t < 4; t++) {
    for (int i = 0; i < primes.size(); i++) {
      masked[i].n = i;
      masked[i].mask = getTripleOfInt(primes[i], t);
    }
    sort(masked.begin(), masked.end(), IntMaskLess());

    for (int i = 0; i < masked.size(); ) {
      int i_hi = upper_bound(masked.begin(), masked.end(), masked[i],
                             IntMaskLess()) - masked.begin();
      if (i_hi - i > 1) {
        // printf("for mask %d we have: ", masked[i].mask);
        // for (int q = i; q < i_hi; q++) {
        //   printf("%d ", primes[masked[q].n]);
        // }
        // printf("\n");

        for (int j = i; j < i_hi; j++) {
          for (int k = i; k < i_hi; k++) {
            if (k == j) continue;
            g[masked[j].n].push_back(masked[k].n);
            g[masked[k].n].push_back(masked[j].n);
            // printf("connect %d with %d\n", primes[masked[k].n], primes[masked[j].n]);
          }
        }
      }
      i = i_hi;
    }
  }

  int test_num = 0;
  scanf("%d", &test_num);
  for (int t = 1; t <= test_num; t++) {
    int s, d;
    scanf("%d%d", &s, &d);
    int cost = findMinimalCost(s, d);
    if (cost < 0) {
      printf("Impossible\n");
    } else {
      printf("%d\n", cost);
    }
  }
  return 0;
}
