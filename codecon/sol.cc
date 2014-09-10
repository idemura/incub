#include <algorithm>
#include <functional>
#include <map>
#include <string>
#include <queue>
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

const int kMax = 300;
const int kCoins[] = {
    5/5, 10/5, 20/5, 50/5, // Cents
    100/5, 200/5, 500/5, 1000/5, 2000/5, 5000/5,
    10000/5 };  // 100$

int mem[ARRAY_SIZEOF(kCoins)][kMax * 100 / 5 + 1];

int countRec(int n, int ci) {
  if (ci == 0 || n == 0) {
    return 1;
  }
  if (mem[ci][n] == 0) {
    int max_coins = n / kCoins[ci];
    int s = 0;
    for (int i = 0; i <= max_coins; i++) {
      s += countRec(n - i * kCoins[ci], ci - 1);
    }
    mem[ci][n] = s;
  }
  return mem[ci][n];
}

int count(int n) {
  return countRec(n, ARRAY_SIZEOF(kCoins) - 1);
}

int main(int argc, char **argv) {
#ifndef ONLINE_JUDGE
  freopen("in", "r", stdin);
#endif
  printf("%d\n", count(200/5));
  return 0;
}

