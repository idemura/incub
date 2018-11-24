#include <algorithm>
#include <assert.h>
#include <ctype.h>
#include <functional>
#include <limits.h>
#include <map>
#include <math.h>
#include <queue>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <vector>

#define ARRAY_SIZEOF(A) (sizeof(A) / sizeof(A[0]))
#define INF 0x7fffffff
#define NON_COPYABLE(C)                                                        \
    C(const C &);                                                              \
    C &operator=(const C &);

using namespace std;

typedef long long int lli;

struct Point {
    int x;
    int left;
    Point(): x(), left() {}
};

struct PointCmp {
    bool operator()(const Point &l, const Point &r) const {
        return l.x < r.x;
    }
};

void readAndSolve() {
    int l, r, n;
    scanf("%d%d%d", &l, &r, &n);
    vector<Point> pts;
    pts.resize(2 * n);
    for (int i = 0; i < n; i++) {
        scanf("%d%d", &pts[2 * i].x, &pts[2 * i + 1].x);
        pts[2 * i].left = 1;
    }
    sort(pts.begin(), pts.end(), PointCmp());
    int min_c = 1000000001, max_c = 0;
    for (int c = 0, i = 0, x = pts[i].x;;) {
        int m = c;
        do {
            if (pts[i].left) {
                m++;
                c++;
            } else {
                c--;
            }
            i++;
        } while (i < pts.size() && pts[i].x == x);

        if (i == pts.size()) break;
        if (l <= x && x <= r) {
            if (m > max_c) max_c = m;
        }
        if (l < pts[i].x && x < r) {
            if (c < min_c) min_c = c;
        }
        x = pts[i].x;
    }
    if (l < pts[0].x || r > pts[pts.size() - 1].x) {
        min_c = 0;
    }
    printf("%d %d\n", min_c, max_c);
}

int main(int argc, char **argv) {
#ifndef ONLINE_JUDGE
    freopen("in", "r", stdin);
    int tests = 1;
#else
    int tests = 10;
#endif
    for (int i = 0; i < tests; i++) {
        readAndSolve();
    }
    return 0;
}
