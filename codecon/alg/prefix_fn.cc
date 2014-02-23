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
  printf("   ");
  for (int i = 0; s[i]; i++) {
    printf("%2c ", s[i]);
  }
  printf("\n");
  printf("%2d ", pf[0]);
  for (int i = 0; s[i]; i++) {
    printf("%2d ", pf[i + 1]);
  }
  printf("\n");
}

bool isPrefix(const char *s, int substr_len, int prefix_len)
{
  const char *suffix = s + substr_len - prefix_len;
  // printf("str: %s\n", s);
  // printf("suffix: %s\n", suffix);
  for (int i = 0; i < prefix_len; i++) {
    if (s[i] != suffix[i]) {
      return false;
    }
  }
  return true;
}

int* naivePrefixFn(const char *s)
{
  const int pf_size = strlen(s) + 1;
  int *pf = new int[pf_size]();
  for (int i = 0; s[i]; i++) {
    // printf("--------\n");
    // printf("index in str %d: %s\n", i, s + i);
    // Check prefixes of length 1,...i.
    int l = 0;
    for (int j = 1; j <= i; j++) {
      // printf("check length %d\n", j);
      if (isPrefix(s, i + 1, j)) {
        l = j;
      }
    }
    // printf("value at %d: %d\n", i + 1, l);
    pf[i + 1] = l;
  }
  return pf;
}

// Computes prefix function `pf` for a string `s`. Memory allocated should be
// freed with `freePrefixFn`.
// `pf[i]` is maximum length of an non-trivial prefix which is also a suffix
// of string `s[0..i-1]` of length `i`:
//    s[j] == s[sf+j], j=0 .. pf[i]-1,
// where `sf=i-pf[i]` and stands for "suffix first".
int* prefixFn(const char *s)
{
  const int pf_size = strlen(s) + 1;
  // Important: `pf` initialized with zeros.
  int *pf = new int[pf_size]();
  for (int i = 2; i < pf_size; i++) {
    for (int k = pf[i - 1]; ; k = pf[k]) {
      if (s[i - 1] == s[k]) {
        pf[i] = k + 1;
        break;
      }
      if (k == 0) break;
    }
  }
  return pf;
}

void freePrefixFn(int *pf)
{
  delete[] pf;
}

void testPrefixFn(const char *s)
{
  const int pf_size = strlen(s) + 1;
  int *pf = prefixFn(s);
  int *pf_naive = naivePrefixFn(s);
  printf("Prefix function:\n");
  printPrefixFn(s, pf);
  printf("CHECK:\n");
  printPrefixFn(s, pf_naive);
  for (int i = 0; i < pf_size; i++) {
    if (pf[i] != pf_naive[i]) {
      printf("*** PF distinct at %d\n", i);
      break;
    }
  }
  freePrefixFn(pf);
  freePrefixFn(pf_naive);
}

int main(int argc, char **argv)
{
// #ifndef ONLINE_JUDGE
//   freopen("in", "r", stdin);
// #endif
  testPrefixFn("ababdaab");
  testPrefixFn("abcabdabcabcx");
  return 0;
}
