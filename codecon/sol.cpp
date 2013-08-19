#include <algorithm>
#include <vector>
#include <map>
#include <utility>
#include <cstdio>
#include <cstring>
#include <cstdlib>
#include <cassert>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define INF 0x7fffffff
#define MOD 1000000007

using namespace std;

#define DIM 5000

struct point {
    int x, c, i, left;
};

bool sort_pred(const point& p1, const point& p2)
{
    if (p1.x == p2.x) {
        return p1.i < p2.i;
    } else {
        return p1.x < p2.x;
    }
}

bool ord(int n1, int n2)
{
    return n1 > n2;
}

point ps[2*DIM+2];
int cs[2*DIM+2];
int ps_n;

void add(int *k, int l, int r, int c, int i)
{
    int j = *k;
    ps[j].x = l;
    ps[j].c = c;
    ps[j].left = 1;
    ps[j].i = i;
    j++;
    ps[j].x = r;
    ps[j].c = c;
    ps[j].left = 0;
    ps[j].i = i;
    j++;
    *k = j;
}

int main()
{
#ifndef ONLINE_JUDGE
    freopen("in", "r", stdin);
#endif
    int i, j = 0;

    add(&j, 1, 1000000000, 1, 0);
    scanf("%d", &ps_n);
    for (i = 0, j = 0; i < ps_n; i++) {
        int l, r;
        char c;
        scanf("%d%d %c", &l, &r, &c);
        add(&j, l, r, c == 'w', i+1);
    }
    int n = j;
    sort(ps, ps + n, sort_pred);
    map<int, int> cmap;
    for (i = 0; i < n-1; i++) {
        if (ps[i].left) {
            cmap[-ps[i].i] = ps[i].c;
        } else {
            assert(cmap.find(-ps[i].i) != cmap.end());
            cmap.erase(cmap.find(-ps[i].i));
        }
        cs[i] = cmap.begin()->second;
    }
    int s = ps[0].x;
    int e = s;
    int s_best = s;
    int e_best = e;
    for (i = 0; i < n-1; i++) {
        if (e == ps[i].x && (cs[i] || ps[i].x == ps[i+1].x)) {
            e = ps[i+1].x;
        } else {
            if (e - s > e_best - s_best) {
                e_best = e;
                s_best = s;
            }
            s = e = ps[i+1].x;
        }
    }
    if (e - s > e_best - s_best) {
        e_best = e;
        s_best = s;
    }
    if (s_best == e_best) {
        s_best--;
    }
    printf("%d %d\n", s_best, e_best);
    return 0;
}
