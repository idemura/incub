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

int kmpSearch(const char *s, const char *n)
{
  int *pf = new int[strlen(n)]();
  computePrefixFn(n, pf);
  int i = 0, j = 0;
  for (; s[i] && n[j];) {
    // Increase both if match.
    if (s[i] == n[j]) {
      i++;
      j++;
      continue;
    }
    if (j == 0) {
      i++;
    } else {
      // Try smaller prefix at the same position.
      j = pf[j - 1];
    }
  }
  delete[] pf;
  return n[j]? -1: i - j;
}

int strStrSearch(const char *s, const char *n)
{
  const char *p = strstr(s, n);
  return p? p - s: -1;
}

void test(const char *s, const char *n)
{
  int i1 = kmpSearch(s, n);
  int i2 = strStrSearch(s, n);
  printf("Search %s in %s\n", n, s);
  printf("  KMP: %d check: %d\n", i1, i2);
  if (i1 != i2) {
    printf("FAILED\n");
  }
}

int main(int argc, char **argv)
{
// #ifndef ONLINE_JUDGE
//   freopen("in", "r", stdin);
// #endif
  test("abcabcabaad", "abc");
  test("abcabcabaad", "abca");
  test("abcabcabaad", "cab");
  test("abcabcabaad", "bcab");
  test("abcabcabaad", "aad");
  test("abcabcabaad", "add");
  return 0;
}
