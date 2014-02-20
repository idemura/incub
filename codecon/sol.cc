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

// Computes prefix function for string `s` and saves into `pf`, which should
// be allocated before the call.
//   pf[i] is max length of an own prefix which is also the suffix of string
//   s[0..i], length of `i+1`.
void computePrefixFn(const char *s, int *pf)
{
  if (*s == 0) {
    return;
  }
  pf[0] = 0;
  printPrefixFn(s, pf);
  for (int i = 1; s[i]; i++) {
    printf("[%d] char %c\n", i, s[i]);
    int k = pf[i - 1];
    printf("k %d pf[%d]\n", k, i - 1);
    for (; k != 0 && s[i] != s[k];) {
      // Because for string of length `k` we store prefix function value at
      // index k - 1.
      printf("check smaller suffix at [%d]\n", k - 1);
      k = pf[k - 1];
      printf("new k %d\n", k);
    }
    pf[i] = k + (s[i] == s[k]);
    printf("pf value is %d\n", pf[i]);
    printPrefixFn(s, pf);
  }
}

void testPrefixFn(const char *s)
{
  int *pf = new int[strlen(s)]();
  computePrefixFn(s, pf);
  printPrefixFn(s, pf);
  delete[] pf;
}

int main(int argc, char **argv)
{
// #ifndef ONLINE_JUDGE
//   freopen("in", "r", stdin);
// #endif
  const char s1[] = "ababdaab";
  testPrefixFn(s1);
  return 0;
}
