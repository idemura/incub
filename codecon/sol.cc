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

const int kCoins[] = { 
    5/5, 10/5, 20/5, 50/5, // Cents
    100/5, 200/5, 500/5, 1000/5, 2000/5, 5000/5,
    10000/5 };  // 100$

void printCoins() {
  for (int i = 0; i < ARRAY_SIZEOF(kCoins); i++) {
    printf("%d ", kCoins[i]);
  }
  printf("\n");
}

int stupidCountRec(int n, int ci, int depth) {
  char tab[24] = {};
  for (int i = 0; i < depth; i++)
    tab[i] = ' ';

  printf("%sn %d ci %d value %d\n", tab, n, ci, kCoins[ci]);
  if (ci == 0 || n == 0) {
    printf("%sci is 0, return 1\n", tab);
    return 1;
  }
  int max_coins = n / kCoins[ci];
  printf("%smax_coins %d\n", tab, max_coins);
  int s = 0;
  for (int i = 0; i <= max_coins; i++) {
    s += stupidCountRec(n - i * kCoins[ci], ci - 1, depth + 1);
    printf("%snew s %d\n", tab, s);
  }
  printf("%sreturn %d\n", tab, s);
  return s;
}

int stupidCount(int n) {
  return stupidCountRec(n, ARRAY_SIZEOF(kCoins) - 1, 0);
}

int main(int argc, char **argv) {
#ifndef ONLINE_JUDGE
  freopen("in", "r", stdin);
#endif

  printCoins();
  printf("%d\n", stupidCount(200/5));
  return 0;
}

