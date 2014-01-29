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
typedef std::vector<std::string> route_vec;

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
std::vector<route_vec> rm;
int rm_ix[84][84];

route_vec* newRouteV(int i, int j)
{
  rm_ix[i][j] = rm.size();
  rm.push_back(route_vec());
  return &rm.back();
}

int routeRec(int i, int j, int iout)
{
  if (rm_ix[i][j]) {
    return rm_ix[i][j];
  }

  if (!lcs_mem[i][j]) {
    return 0;
  }
  int lcs_ij = lcs_mem[i][j];
  if (a[i - 1] == b[j - 1]) {
    int rvi = routeRec(i - 1, j - 1, iout + 1);
    route_vec &rv = rm[rvi];
    route_vec *new_rv = newRouteV(i, j);
    for (int i = 0; i < rv.size(); i++) {
      new_rv->push_back(rv[i] + a[i - 1]);
    }
  } else {
    if (lcs_mem[i - 1][j] == lcs_mem[i][j - 1]) {
      int rmi1 = routeRec(i - 1, j, iout);
      int rmi2 = routeRec(i, j - 1, iout);
      route_vec *new_rv = newRouteV(i, j);
      new_rv->insert(new_rv->end(), rm[rmi1].begin(), rm[rmi1].end());
      new_rv->insert(new_rv->end(), rm[rmi2].begin(), rm[rmi2].end());
    } else {
      int rmi = 0;
      if (lcs_mem[i - 1][j] == lcs_ij) {
        rmi = routeRec(i - 1, j, iout);
      }
      if (lcs_mem[i][j - 1] == lcs_ij) {
        rmi = routeRec(i, j - 1, iout);
      }
      assert(rmi != 0);
      rm_ix[i][j] = rmi;
    }
  }
  return lcs_mem[i][j];
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
  // revert(a, a + an - 1);
  // revert(b, b + bn - 1);
  lcs(a, b);
  memset(rm_ix, 0, sizeof rm_ix);
  rm.push_back(route_vec());
  routeRec(an, bn, 0);
  route_vec rv = rm[rm_ix[an][bn]];
  std::sort(rv.begin(), rv.end());
  rv.erase(std::unique(rv.begin(), rv.end()), rv.end());
  for (int i = 0; i < rv.size(); i++) {
    printf("%s\n", rv[i].c_str());
  }
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
    routes();
  }
  return 0;
}
