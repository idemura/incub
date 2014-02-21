#include <algorithm>
#include <map>
#include <string>
#include <vector>
#include <utility>
#include <math.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))

using namespace std;

void printIntVec(const vector<int> &v)
{
  for (int i = 0; i < v.size(); i++) {
    printf("%d ", v[i]);
  }
  printf("of size %zu\n", v.size());
}

int lis(const vector<int> &as, vector<int> *lis_out)
{
  vector<int> top;
  for (int i = 0; i < as.size(); i++) {
    // `top` maintained sorted all the time. `top[k]` is the minimal tail
    // element of increasing sequence of length `k`. Can prove that adding
    // to the maximum tail is the optimal strategy.
    int x = as[i];
    int l = 0, r = top.size();
    while (l < r) {
      int m = (r + l) / 2;
      if (x <= top[m]) {
        r = m;
      } else {
        l = m + 1;
      }
    }
    // l is the index of an item >= x.
    if (l == top.size()) {
      top.push_back(as[i]);
    } else {
      top[l] = x;
    }
  }
  lis_out->assign(top.begin(), top.end());
  return 0;
}

int main()
{
  const int vs[] = {10, 30, 20, 15, 5, 8, 25, 40, 50};
  vector<int> v(vs, vs + ARRAY_SIZEOF(vs));
  vector<int> lis_out;
  lis(v, &lis_out);
  printf("LIS:\n");
  printIntVec(lis_out);
  return 0;
}
