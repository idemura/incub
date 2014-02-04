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

const int DIM = 1008;

int status[DIM];
int gen[DIM];
int closeAll, openCount;

int main(int argc, char **argv)
{
// #ifndef ONLINE_JUDGE
//   freopen("in", "r", stdin);
// #endif
  char line[80] = {};
  int n, k;
  fgets(line, 80, stdin);
  sscanf(line, "%d%d", &n, &k);
  for (int i = 0; i < k; i++) {
    fgets(line, 80, stdin);
    if (line[5] == ' ') {
      int ix = 0;
      sscanf(line + 5, "%d", &ix);
      status[ix] = 1 - (gen[ix] >= closeAll? status[ix]: 0);
      gen[ix] = closeAll;
      openCount += status[ix]? 1: -1;
    } else {
      // "CLOSEALL"
      closeAll++;
      openCount = 0;
    }
    printf("%d\n", openCount);
  }
  return 0;
}
