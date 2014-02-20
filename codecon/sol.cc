#include <algorithm>
#include <map>
#include <string>
#include <vector>
#include <utility>
#include <math.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define INF 0x7fffffff

typedef long long int lli;

void printPrefixFn(const char *s, int *pf)
{
  for (int i = 0; s[i]; i++) {
    printf("%2c ", s[i]);
  }
  printf("\n");
  for (int i = 0; s[i]; i++) {
    printf("%2d ", pf[i]);
  }
  printf("\n");
}

// Prefix function from alg/prefix-fn.cc.
void computePrefixFn(const char *s, int *pf)
{
  if (*s == 0) {
    return;
  }
  pf[0] = 0;
  for (int i = 1; s[i]; i++) {
    int k = pf[i - 1];
    for (; k != 0 && s[i] != s[k];) {
      k = pf[k - 1];
    }
    pf[i] = k + (s[i] == s[k]);
  }
}

int kmpSearch(const char *s, const char *n, int *pf)
{
  int i = 0, j = 0;
  while (s[i] && n[j]) {
    if (s[i] == n[j]) {
      i++;
      j++;
      continue;
    }
    if (j == 0) {
      i++;
      continue;
    }
    // Try another (shorter) prefix, possibly of length 0.
    j = pf[j - 1];
  }
  return n[j]? -1: i - j;
}

int substrIndexNaive(const char *s, const char *n)
{
  const char *p = strstr(s, n);
  return p? p - s: -1;
}

void testSubstrSearch(const char *s, const char *n)
{
  const int s_len = strlen(s);
  int *pf = new int[s_len]();
  computePrefixFn(s, pf);
  printf("Prefix function:\n");
  printPrefixFn(s, pf);
  int i1 = kmpSearch(s, n, pf);
  int i2 = substrIndexNaive(s, n);
  printf("Search %s in %s\n", n, s);
  printf("  KMP: %d check: %d\n", i1, i2);
  if (i1 != i2) {
    printf("FAILED!\n");
  }
  delete[] pf;
}

int main(int argc, char **argv)
{
// #ifndef ONLINE_JUDGE
//   freopen("in", "r", stdin);
// #endif
  testSubstrSearch("ababdaab", "abd");
  testSubstrSearch("ababdaab", "aba");
  testSubstrSearch("ababdaab", "aab");
  testSubstrSearch("ababdaab", "aaa");
  return 0;
}
