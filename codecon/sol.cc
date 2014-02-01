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

int counter[26];
int mat[26][26];

// Floyd-Warshall works just good.
void FW()
{
  for (int i = 0; i < 26; i++) {
    for (int j = 0; j < 26; j++) {
      for (int k = 0; k < 26; k++) {
        mat[i][j] = mat[i][j]? 1: mat[i][k] && mat[k][j];
      }
    }
  }
}

int main(int argc, char **argv)
{
// #ifndef ONLINE_JUDGE
//   freopen("in", "r", stdin);
// #endif
  int t = 0, n;
  scanf("%d", &t);
  while (t-- > 0) {
    memset(counter, 0, sizeof counter);
    memset(mat, 0, sizeof mat);
    scanf("%d", &n);
    char buf[1024];
    for (int i = 0; i < n; i++) {
      scanf("%s", buf);
      int i0 = buf[0] - 'a';
      int i1 = buf[strlen(buf)-1] - 'a';
      counter[i0]++;
      counter[i1]--;
      mat[i0][i1] = mat[i1][i0] = 1;
    }
    int n0 = 0, i0 = -1, n1 = 0, i1 = -1, others = 0;
    for (int i = 0; i < 26; i++) {
      if (counter[i] == -1) {
        n0++;
        i0 = i;
      } else if (counter[i] == 1) {
        n1++;
        i1 = i;
      } else if (counter[i] != 0) {
        others++;
      }
    }
    bool possible = false;
    if (n0 <= 1 && n1 <= 1 && others == 0) {
      FW();
      int accessible = 1;
      for (int i = 0; i < 26; i++)
      if
      if (n0 == 1 && n1)
    } && FW()) {
      printf("Ordering is possible.\n");
    } else {
      printf("The door cannot be opened.\n");
    }
  }
  return 0;
}
