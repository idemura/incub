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

using namespace std;

typedef long long int lli;

class SpiralRoute {
public:
  vector<int> thronePosition(int w, int l)
  {
    vector<int> v(2);
    bool swapped = false;
    if (w > l) {
      swap(w, l);
      swapped = true;
    }
    if (w % 2) {
      v[0] = w / 2;
      v[1] = l - v[0] - 1;
      if (swapped) {
        swap(v[0], v[1]);
      }
    } else {
      int h = (w - 2) / 2;
      v[0] = h;
      v[1] = v[0] + 1;
    }
    return v;
  }
};
