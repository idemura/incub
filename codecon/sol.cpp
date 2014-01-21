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
  lli var;

  Point(): x(), max_count(), var() {}
};

Interval ints[100005];
int N;

bool cmpIntervalentB(const Interval &lh, const Interval &rh)
{
  return lh.b == rh.b? lh.a < rh.a: lh.b < rh.b;
}

bool pointXLessThanValue(const Point &lh, int x)
{
  return lh.x < x;
}

void printPoints(const std::vector<Point> &pts)
{
  printf("points:\n");
  for (int i = 0; i < pts.size(); i++) {
    Point p = pts[i];
    printf("  x=%d max_count=%lld var=%lld\n", p.x, p.max_count, p.var);
  }
}

void getMaxCount(int x, const std::vector<Point> &pts, lli *max_count, lli *var)
{
  printf("getMaxCount x=%d\n", x);
  printPoints(pts);

  if (pts.empty() || x <= pts[0].x) {
    printf("pts are empty or x <= first x\n");
    *max_count = 0;
    *var = 1;
  } else {
    std::vector<Point>::const_iterator lb =
        std::lower_bound(pts.begin(), pts.end(), x, pointXLessThanValue);
    assert(lb != pts.begin());
    printf("found lower bound\n");
    // We checked `x <= pts[0].x`, so `lb != pts.begin()`.
    if (lb == pts.end() || lb->x != x) {
      lb--;
      printf("  ...one step back\n");
    }
    // After this `lb != pts.end()`.
    assert(lb != pts.end());
    printf("lb->x=%d\n", lb->x);
    *max_count = lb->max_count;
    *var = lb->var;
  }
}

int main(int argc, char **argv)
{
#ifndef ONLINE_JUDGE
    freopen("in", "r", stdin);
#endif
  scanf("%d", &N);
  for (int i = 0; i < N; i++) {
    scanf("%d%d", &ints[i].a, &ints[i].b);
  }
  // Sort by right border of an interval.
  std::sort(ints, ints + N, cmpIntervalentB);
  printf("sorted:\n");
  for (int i = 0; i < N; i++) {
    printf("  a=%d b=%d\n", ints[i].a, ints[i].b);
  }

  // For every Intervalent, find such that it's right is less or equal of i-th
  // Intervalent's left point. They don't intersect.
  std::vector<Point> pts;
  for (int i = 0; i < N; i++) {
    printf("\n");
    printf("step %d\n", i);
    lli max_count, var;
    getMaxCount(ints[i].a, pts, &max_count, &var);
    printf("max_count=%lld var=%lld\n", max_count, var);
    max_count += 1;
    printf("max_count inc\n");
    if (pts.empty() || pts[pts.size() - 1].x < ints[i].b) {
      Point p;
      p.x = ints[i].b;
      p.max_count = max_count;
      p.var = var;
      printf("adding new point:\n");
      printf("  x=%d\n", p.x);
      printf("  max_count=%lld\n", p.max_count);
      printf("  var=%lld\n", p.var);
      pts.push_back(p);
    } else {
      // So, the last point x is equal to the ints[i].b.
      Point &last = pts[pts.size() - 1];
      printf("last point:\n");
      printf("  x=%d\n", last.x);
      printf("  max_count=%lld\n", last.max_count);
      printf("  var=%lld\n", last.var);
      if (last.max_count < max_count) {
        printf("last.max_count is less than max_count=%lld var=%lld set new.\n",
               max_count, var);
        last.max_count = max_count;
        last.var = var;
      } else if (last.max_count == max_count) {
        last.var += var;
        printf("same max_count, var=%lld added, now %lld\n", var, last.var);
      }
      printf("end else.\n");
    }
  }
  printf("%lld\n", pts[pts.size() - 1].var);
  return 0;
}
