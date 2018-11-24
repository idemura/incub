#include <algorithm>
#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <map>
#include <utility>
#include <vector>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define INF 0x7fffffff
#define MOD 1000000007

using namespace std;

#define DIM 5001

int x0[DIM];
int x1[DIM];
int cs[DIM];
int n_seg;
int xs[2 * DIM];
int res_cs[2 * DIM];

int main() {
#ifndef ONLINE_JUDGE
    freopen("in", "r", stdin);
#endif
    int i, j;

    scanf("%d", &n_seg);
    n_seg++;

    x0[0] = 0;
    x1[0] = 1000000000;
    cs[0] = 1;
    for (i = 1; i < n_seg; i++) {
        char c;
        scanf("%d%d %c", &x0[i], &x1[i], &c);
        cs[i] = c == 'w';
    }
    for (i = 0; i < n_seg; i++) {
        xs[2 * i] = x0[i];
        xs[2 * i + 1] = x1[i];
    }
    int n_pts = 2 * n_seg;
    sort(xs, xs + 2 * n_seg);
    j = 1;
    for (i = 1; i < n_pts; i++) {
        if (xs[i - 1] != xs[i]) {
            xs[j++] = xs[i];
        }
    }
    n_pts = j;
    for (i = 0; i < n_seg; i++) {
        // Find lower bound.
        int l = 0, u = n_pts;
        while (l < u) {
            int m = (l + u) / 2;
            if (x0[i] > xs[m]) {
                l = m + 1;
            } else {
                u = m;
            }
        }
        // assert(xs[l] == x0[i]);
        for (j = l; xs[j] < x1[i]; j++) {
            res_cs[j] = cs[i];
        }
    }
    int x = 0, len = 0, max_len = -1;
    for (i = 0; i < n_pts - 1; i++) {
        if (res_cs[i]) {
            len += xs[i + 1] - xs[i];
            if (len > max_len) {
                max_len = len;
                x = xs[i + 1];
            }
        } else {
            len = 0;
        }
    }
    printf("%d %d\n", x - max_len, x);
    return 0;
}
