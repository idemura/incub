#include <algorithm>
#include <map>
#include <string>
#include <vector>
#include <utility>
#include <math.h>
#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define INF 0x7fffffff

typedef long long int lli;

const int kMax16BitInt = 0xffff;

int countBits16Linear(int n)
{
  int c = 0;
  for (int i = 0; i < 16; i++) {
    if (n & (1u << i)) {
      c++;
    }
  }
  return c;
}

// Input: 32 bit unsigned integer.
int countBits32(unsigned int n) {
  // Treat `n` as sums of groups of 1 bit.
  n = (n & 0x55555555u) + ((n & 0xaaaaaaaau) >> 1);
  // `n` is now sum of groups of 2 bits and so on on next steps.
  n = (n & 0x33333333u) + ((n & 0xccccccccu) >> 2);
  n = (n & 0x0f0f0f0fu) + ((n & 0xf0f0f0f0u) >> 4);
  n = (n & 0x00ff00ffu) + ((n & 0xff00ff00u) >> 8);
  n = (n & 0x0000ffffu) + (n >> 16);
  return (int)n;
}

int main(int argc, char **argv)
{
// #ifndef ONLINE_JUDGE
//   freopen("in", "r", stdin);
// #endif
  for (int i = 0; i <= kMax16BitInt; i++) {
    int c1 = countBits32(i);
    int c2 = countBits16Linear(i);
    if (c1 != c2) {
      printf("*** Count bits: %d, check %d\n", c1, c2);
    }
  }
  return 0;
}
