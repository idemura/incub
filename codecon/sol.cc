#include <algorithm>
#include <map>
#include <set>
#include <string>
#include <vector>
#include <utility>
#include <assert.h>
#include <ctype.h>
#include <limits.h>
#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

#define ARRAY_SIZEOF(A) (sizeof(A) / sizeof(A[0]))
#define INF 0x7fffffff
#define NON_COPYABLE(C) \
    C(const C&); \
    C& operator=(const C&);

typedef long long int lli;

int main(int argc, char **argv)
{
#ifndef ONLINE_JUDGE
  freopen("in", "r", stdin);
#endif
  return 0;
}
