// See http://uva.onlinejudge.org/external/1/103.html
#include <algorithm>
#include <memory.h>
#include <stdio.h>
#include <utility>
#include <vector>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))

using namespace std;

typedef long long int lli;

struct pix { // Pointer IndeX
    int *p, i;
};

int n, k;
int vs_data[30][10];
pix vs[30];
int ll[30];
int pl[30];
int rs[30];

bool ins(const pix &v1, const pix &v2) {
    for (int i = 0; i < n; i++) {
        if (!(v1.p[i] < v2.p[i])) return false;
    }
    return true;
}

bool lex(const pix &v1, const pix &v2) {
    for (int i = 0; i < n; i++) {
        if (v1.p[i] != v2.p[i]) return v1.p[i] > v2.p[i];
    }
    return false;
}

int main() {
#ifndef ONLINE_JUDGE
    freopen("in", "r", stdin);
#endif
    int i, j;

    while (scanf("%d%d", &k, &n) == 2) {
        memset(ll, 0, sizeof ll);
        memset(pl, 0, sizeof pl);

        for (i = 0; i < k; i++) {
            vs[i].p = vs_data[i];
            vs[i].i = i + 1;
            for (j = 0; j < n; j++) {
                scanf("%d", vs[i].p + j);
            }
            sort(vs[i].p, vs[i].p + n);
        }
        sort(vs, vs + k, lex);

        int vmax = 0;
        ll[0] = 1;
        pl[0] = 0;
        for (i = 1; i < k; i++) {
            int jmax = -1;
            for (j = 0; j < i; j++) {
                if (ins(vs[i], vs[j]) && (jmax < 0 || ll[j] > ll[jmax])) {
                    jmax = j;
                }
            }
            if (jmax >= 0) {
                ll[i] = ll[jmax] + 1;
                pl[i] = jmax + 1;
            } else {
                ll[i] = 1;
                pl[i] = 0;
            }
            if (ll[i] > ll[vmax]) {
                vmax = i;
            }
        }

        const int lmax = ll[vmax];
        printf("%d\n", lmax);
        for (i = 0; i < lmax; i++) {
            printf("%d ", vs[vmax].i);
            vmax = pl[vmax] - 1;
        }
        printf("\n");
    }
    return 0;
}
