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

int main(int argc, char **argv)
{
// #ifndef ONLINE_JUDGE
//   freopen("in", "r", stdin);
// #endif
  int t = 0;
  scanf("%d", &t);
  int test_case[102], max = 0;
  for (int i = 0; i < t; i++) {
    scanf("%d", &test_case[i]);
    if (test_case[i] > max) {
      max = test_case[i];
    }
  }
  ln[1][0] = ln_n[1] = 1;
  for (int i = 2; i <= max; i++) {
    mult(i - 1, i, i);
  }
  for (int i = 0; i < t; i++) {
    printNum(test_case[i]);
  }
  return 0;
}
