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

bool isPrefix(const char *s, int substr_len, int prefix_len)
{
  int suffix_first = substr_len - prefix_len;
  for (int i = 0; i < prefix_len; i++) {
    if (s[i] != s[suffix_first + i]) {
      return false;
    }
  }
  return true;
}

void naivePrefixFn(const char *s, int *pf)
{
  // Fill every index of prefix function. pf[i] is for the left substring of
  // length `i+1`.
  for (int i = 0; s[i]; i++) {
    // Check prefixes of length 0,1,...i.
    int l = 0;
    for (int j = 0; j <= i; j++) {
      if (isPrefix(s, i + 1, j)) {
        l = j;
      }
    }
    pf[i] = l;
  }
}

// Computes prefix function for a string `s` and saves into `pf`, which is
// allocated before the call. `pf[i]` is maximum length of an own (non-trivial)
// prefix which is also a suffix of string `s[0..i]`, of length `i+1`:
//    s[j] == s[sf+j], j=0 .. pf[i]-1,
// where `sf=i+1-pf[i]` and stands for "suffix first".
void computePrefixFn(const char *s, int *pf)
{
  if (*s == 0) {
    return;
  }
  pf[0] = 0;
  for (int i = 1; s[i]; i++) {
    int k = pf[i - 1];
    for (; k != 0 && s[i] != s[k];) {
      // Because for string of length `k` we store prefix function value at
      // index k - 1.
      k = pf[k - 1];
    }
    pf[i] = k + (s[i] == s[k]);
  }
}

void testPrefixFn(const char *s)
{
  const int s_len = strlen(s);
  int *pf = new int[s_len]();
  int *pf_naive = new int[s_len]();
  computePrefixFn(s, pf);
  printf("Prefix function:\n");
  printPrefixFn(s, pf);
  printf("CHECK:\n");
  naivePrefixFn(s, pf_naive);
  printPrefixFn(s, pf_naive);
  for (int i = 0; i < s_len; i++) {
    if (pf[i] != pf_naive[i]) {
      printf(" PF distinct at %d\n", i);
      break;
    }
  }
  delete[] pf;
  delete[] pf_naive;
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
