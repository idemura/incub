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
#define MOD 1000000007

int rems[5001];

int main()
{
#ifndef ONLINE_JUDGE
  freopen("in", "r", stdin);
#endif
  int a, b;
  scanf("%d%d", &a, &b);
  if (a >= b) {
    printf("%d.", a / b);
    a %= b;
  } else {
    printf("0.");
  }

  std::vector<int> dds;
  while (rems[a] == 0) {
    rems[a] = dds.size() + 1;
    int d = 10 * a / b;
    int x = 10 * a % b;
    dds.push_back(d);
    a = x;
  }

  const int cycle = rems[a] - 1;
  int i = 0;
  for (; i < cycle; i++) {
    printf("%d", dds[i]);
  }
  printf("(");
  for (; i < dds.size(); i++) {
    printf("%d", dds[i]);
  }
  printf(")\n");
  return 0;
}
