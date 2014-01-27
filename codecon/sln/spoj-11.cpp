#include <algorithm>
#include <map>
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

int factorialTrailingZeroesNum(int n)
{
  int nz = 0;
  int p5 = 5;
  int max_p5 = INF / 5;
  for (; p5 <= n; p5 *= 5) {
    nz += n / p5;
    if (p5 >= max_p5) {
      break;
    }
  }
  return nz;
}

int main(int argc, char **argv)
{
// #ifndef ONLINE_JUDGE
//   freopen("in", "r", stdin);
// #endif
  int T = 0, n;
  scanf("%d", &T);
  for (; T-- > 0; ) {
    scanf("%d", &n);
    printf("%d\n", factorialTrailingZeroesNum(n));
  }
  return 0;
}
