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

static const int M = 12;  // Margin.
static const int kFree = 1;
static const int kMark = 2;

int m[1024][1024];
int mr, mc;

int max4(int *d)
{
  int *r = d;
  if (d[1] > *r)
    r = d + 1;
  if (d[2] > *r)
    r = d + 2;
  if (d[3] > *r)
    r = d + 3;
  return r - d;
}

// Here diameter of the tree will be after DFS.
int s_diam = 0;
int dfs(int i, int j)
{
  printf("dfs %d %d\n", i-M, j-M);
  m[i][j] = kMark;
  int d[4] = {};
  if (m[i-1][j] == kFree) {
    printf("i-1: %d %d\n", i-1-M, j-M);
    d[0] = dfs(i-1, j);
    printf("  dfs=%d\n", d[0]);
  }
  if (m[i+1][j] == kFree) {
    printf("i+1: %d %d\n", i+1-M, j-M);
    d[1] = dfs(i+1, j);
    printf("  dfs=%d\n", d[1]);
  }
  if (m[i][j-1] == kFree) {
    printf("j-1: %d %d\n", i-M, j-1-M);
    d[2] = dfs(i, j-1);
    printf("  dfs=%d\n", d[2]);
  }
  if (m[i][j+1] == kFree) {
    printf("j-1: %d %d\n", i-M, j+1-M);
    d[3] = dfs(i, j+1);
    printf("  dfs=%d\n", d[3]);
  }
  int maxi = max4(d);
  printf("  max 1: %d\n", d[maxi]);
  int max_depth = d[maxi];
  d[maxi] = 0;  // Zero to reveal next max value.
  printf("  max 2: %d\n", d[max4(d)]);
  int diam = max_depth + d[max4(d)];
  printf("diam %d\n", diam);
  if (diam > s_diam) {
    printf("update diam %d\n", diam);
    s_diam = diam;
  }
  m[i][j] = kFree;
  return max_depth + 1;
}

// From problem statement I conclude labyrinth is a tree.
int main(int argc, char **argv)
{
// #ifndef ONLINE_JUDGE
//   freopen("in", "r", stdin);
// #endif
  int T = 0;
  scanf("%d", &T);
  for (; T-- > 0; ) {
    printf("-------- TEST %d\n", T+1);
    scanf("%d%d", &mr, &mc);
    char ln[1024];
    int i0, j0;
    for (int i = 0, im = M; i < mr; i++, im++) {
      scanf("%s", ln);
      for (int j = 0, jm = M; j < mc; j++, jm++) {
        m[im][jm] = ln[j] == '.';
        if (m[im][jm]) {
          i0 = im;
          j0 = jm;
        }
      }
    }
    for (int i = 0; i < mr; i++) {
      for (int j = 0; j < mc; j++) {
        printf("%d ", m[M+i][M+j]);
      }
      printf("\n");
    }

    printf("i0=%d j0=%d\n", i0, j0);
    dfs(i0, j0);
    printf("Maximum rope length is %d.\n", s_diam);
  }
  return 0;
}
