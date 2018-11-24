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

struct Rect {
    int x0, y0, x1, y1;
    Rect(): x0(), y0(), x1(), y1() {}
};

struct Segm {
    int x0, x1;
    Segm(): x0(), x1() {}
};

typedef multimap<int, Rect *> YRectMap;

int getMaxSq(const YRectMap &rects, int x, int y) {
    if (rects.empty()) {
        return x * y;
    }
    vector<Segm> segms;
    segms.push_back(Segm());
    segms[0].x0 = 0;
    segms[0].x1 = x;
    int sq_max = 0;
    for (YRectMap::const_iterator i = rects.begin(); i != rects.end(); ++i) {
        Rect *r = i->second;
        vector<Segm> temp;
        Segm s;
        for (int j = 0; j < segms.size(); j++) {
            if (segms[j].x1 <= r->x0 || segms[j].x0 >= r->x1) {
                // No overlap.
                temp.push_back(segms[j]);
            } else {
                int sq = (y - r->y1) * (segms[j].x1 - segms[j].x0);
                if (sq > sq_max) sq_max = sq;

                if (segms[j].x0 < r->x0) {
                    s.x0 = segms[j].x0;
                    s.x1 = r->x0;
                    temp.push_back(s);
                }
                if (r->x1 < segms[j].x1) {
                    s.x0 = r->x1;
                    s.x1 = segms[j].x1;
                    temp.push_back(s);
                }
            }
        }
        segms.swap(temp);
    }
    for (int j = 0; j < segms.size(); j++) {
        int sq = y * (segms[j].x1 - segms[j].x0);
        if (sq > sq_max) sq_max = sq;
    }
    return sq_max;
}

void readAndSolve() {
    int n, r_num;
    scanf("%d%d", &n, &r_num);
    vector<Rect> rects(r_num);
    for (int i = 0; i < rects.size(); i++) {
        Rect &r = rects[i];
        scanf("%d%d%d%d", &r.x0, &r.x1, &r.y0, &r.y1);
    }
    // May be we can just re-use `rects`.
    YRectMap lines;
    for (int i = 0; i < rects.size(); i++) {
        lines.insert(make_pair(rects[i].y0, &rects[i]));
    }

    YRectMap top_sorted;
    int sq_max = 0;
    for (YRectMap::const_iterator i = lines.begin(); i != lines.end(); i++) {
        Rect *r = i->second;
        int sq = getMaxSq(top_sorted, n, r->y0);
        // Instead of using `greater` invert to negative.
        top_sorted.insert(make_pair(-r->y1, r));
        if (sq > sq_max) sq_max = sq;
    }
    printf("%d\n", sq_max);
}

int main(int argc, char **argv) {
#ifndef ONLINE_JUDGE
    freopen("in", "r", stdin);
#endif

    int t = 0;
    scanf("%d", &t);
    for (int i = 0; i < t; i++) {
        readAndSolve();
    }
    return 0;
}
