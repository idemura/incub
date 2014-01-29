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

int lcs_mem[84][84];
int lcs(char *a, char *b)
{
  memset(lcs_mem, 0, sizeof lcs_mem);
  int an = strlen(a);
  int bn = strlen(b);
  for (int i = 1; i <= an; i++) {
    for (int j = 1; j <= bn; j++) {
      if (a[i - 1] == b[j - 1]) {
        lcs_mem[i][j] = lcs_mem[i - 1][j - 1] + 1;
      } else {
        lcs_mem[i][j] = std::max(lcs_mem[i - 1][j], lcs_mem[i][j - 1]);
      }
    }
  }
  return lcs_mem[an][bn];
}

char a[84], b[84], buf[84];
int cs;

void printLCS()
{
  int an = strlen(a);
  int bn = strlen(b);
  for (int i = 0; i <= an; i++) {
    for (int j = 0; j <= bn; j++) {
      printf("%3d", lcs_mem[i][j]);
    }
    printf("\n");
  }
}

void routeRec(int i, int j, int iout)
{
  printf("In %d %d: %c %c\n", i, j, a[i-1], b[j-1]);
  printf("%2d %2d\n", lcs_mem[i-1][j-1], lcs_mem[i-1][j]);
  printf("%2d %2d\n", lcs_mem[i][j-1], lcs_mem[i][j]);
  if (lcs_mem[i][j] < 0) {
    // Already looked into it.
    printf("return as visited\n");
    printf("------ END\n");
    return;
  }
  if (!lcs_mem[i][j]) {
    buf[iout] = 0;
    cs++;
    printf("print result\n");
    printf("%s\n", buf);
    printf("------ END\n");
    return;
  }
  int lcs_ij = lcs_mem[i][j];
  lcs_mem[i][j] = -1;
  if (a[i - 1] == b[j - 1]) {
    printf("ai == bj\n");
    buf[iout] = a[i - 1];
    routeRec(i - 1, j - 1, iout + 1);
  } else {
    int branches = 0;
    int t = 0;
    // Neighbors 2D indices.
    int neighbor[][2] = {{i - 1, j}, {i, j - 1}};
    if (lcs_mem[i - 1][j] == lcs_mem[i][j - 1]) {
      printf("neighbors are equal. chars: %c %c\n", a[i - 1], b[j - 1]);
      assert(i >= 1 && j >= 1);
      t = b[j - 1] < a[i - 1];
    }
    printf("t=%d, so: %d %d, %d %d\n", t, neighbor[t][0], neighbor[t][1],
           neighbor[1-t][0], neighbor[1-t][1]);
    int *ix = neighbor[t];
    if (lcs_mem[ix[0]][ix[1]] == lcs_ij) {
      printf("check branch 1\n");
      routeRec(ix[0], ix[1], iout);
      branches++;
    }
    ix = neighbor[1 - t];
    if (lcs_mem[ix[0]][ix[1]] == lcs_ij) {
      printf("check branch 2\n");
      routeRec(ix[0], ix[1], iout);
      branches++;
    }
    if (branches > 1) {
      printf("branches %d\n", branches);
    }
  }
  printf("------ END\n");
}

void revert(char *b, char *e)
{
  for (; b < e; b++, e--) {
    std::swap(*b, *e);
  }
}

void routes()
{
  int an = strlen(a);
  int bn = strlen(b);
  revert(a, a + an - 1);
  revert(b, b + bn - 1);
  lcs(a, b);
  printLCS();
  printf("lcs len %d\n", lcs_mem[an][bn]);
  routeRec(an, bn, 0);
}

int main(int argc, char **argv)
{
// #ifndef ONLINE_JUDGE
//   freopen("in", "r", stdin);
// #endif
  int t = 0;
  scanf("%d", &t);
  while (t-- > 0) {
    scanf("%s%s", a, b);
    printf("%s\n%s\n", a, b);
    routes();
    printf("%d\n", cs);
  }
  return 0;
}
