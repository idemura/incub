#include <algorithm>
#include <map>
#include <string>
#include <vector>
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define INF 0x7fffffff

typedef long long int lli;

struct Interval {
  int a, b;
};

struct Point {
  int x;
  lli max_count;
  // How many variants to get `max_count` intervals with `x <= this.x`.
  lli var;

  Point(): x(), max_count(), var() {}
};

Interval ints[100005];
int N;

bool cmpIntervalentB(const Interval &lh, const Interval &rh)
{
  return lh.b == rh.b? lh.a < rh.a: lh.b < rh.b;
}

bool xLessPointX(int x, const Point &p)
{
  return x < p.x;
}

void getMaxCount(int x, const std::vector<Point> &pts, lli *max_count, lli *var)
{
  if (pts.empty() || x <= pts[0].x) {
    *max_count = *var = 1;
  } else {
    // Upper bound will be not equal to begin().
    std::vector<Point>::const_iterator bs =
        std::upper_bound(pts.begin(), pts.end(), x, xLessPointX) - 1;
    *max_count = bs->max_count + 1;
    *var = bs->var;
  }
}

int main(int argc, char **argv)
{
// #ifndef ONLINE_JUDGE
//     freopen("in", "r", stdin);
// #endif
  scanf("%d", &N);
  for (int i = 0; i < N; i++) {
    scanf("%d%d", &ints[i].a, &ints[i].b);
  }
  // Sort by right border of an interval.
  std::sort(ints, ints + N, cmpIntervalentB);

  // For every Intervalent, find such that it's right is less or equal of i-th
  // Intervalent's left point. They don't intersect.
  std::vector<Point> pts;
  Point p;
  p.x = ints[0].b;
  p.max_count = p.var = 1;
  pts.push_back(p);
  for (int i = 1; i < N; i++) {
    lli max_count, var;
    getMaxCount(ints[i].a, pts, &max_count, &var);
    Point last = pts[pts.size() - 1];
    if (last.max_count <= max_count) {
      p.max_count = max_count;
      p.var = last.max_count == max_count? last.var + var: var;
    } else {
      p = last;
    }
    p.x = ints[i].b;
    pts.push_back(p);
  }
  printf("%lld\n", pts[pts.size() - 1].var);
  return 0;
}
