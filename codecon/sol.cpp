#include <algorithm>
#include <vector>
#include <utility>
#include <stdio.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))

using namespace std;

typedef long long int lli;

struct worker {
    int l, r, c;
};
struct status {
    int r;
    lli c;
};

#define MAXN 300

worker ws[100000];
status st[2][MAXN+1];

bool left_less(const worker& j1, const worker& j2) {
    if (j1.l == j2.l)
        return j1.r < j2.r;
    else
        return j1.l < j2.l;
}

int main()
{
#ifndef ONLINE_JUDGE
    freopen("in", "r", stdin);
#endif
    int i, j, k, n, m;
    scanf("%d%d%d", &n, &m, &k);
    for (i = 0; i < m; i++) {
        scanf("%d%d%d", &ws[i].l, &ws[i].r, &ws[i].c);
    }
    int f = 0;
    for (j = 0; j <= n; j++) {
        st[0][j].c = st[1][j].c = -1;
    }
    sort(ws, ws + m, left_less);
    for (i = 0; i < m; i++) {
        printf("%d-%d cost %d\n", ws[i].l, ws[i].r, ws[i].c);
    }
    st[0][0].r = st[1][0].r = 1;
    st[0][0].c = st[1][0].c = 0;
    printf("m %d\n", m);
    for (i = 0; i < m; i++) {
        printf("worker %d-%d cost %d\n", ws[i].l, ws[i].r, ws[i].c);
        for (j = 0; j <= n; j++) {
            printf("j %d r[j] %d\n", j, st[f][j].r);
            if (st[f][j].r <= 0) {
                st[1-f][j].r = st[f][j].r;
                st[1-f][j].c = st[f][j].c;
                printf("skip\n");
                continue;
            }
            int l = max(ws[i].l, st[f][j].r + 1);
            int r = max(ws[i].r, st[f][j].r + 1);
            int len = r - l + 1;
            printf("l %d r %d of len %d\n", l, r, len);
            int new_cov = j + len;
            printf("new_cov %d\n", new_cov);
            lli new_val = st[f][j].c + ws[i].c;
            if (st[f][new_cov].c < 0 || new_val < st[f][new_cov].c) {
                printf("update %d:\n", new_cov);
                st[1-f][new_cov].c = new_val;
                st[1-f][new_cov].r = ws[i].r;
                printf("cost %lld\n", new_val);
                printf("r %d\n", ws[i].r);

                printf("costs:\n");
                for (int q = 0; q <= n; q++) {
                    printf("%lld ", st[1-f][q].c);
                }
                printf("\n");

            } else {
                st[1-f][j].r = st[f][j].r;
                st[1-f][j].c = st[f][j].c;
            }
        }
        printf("old f %d\n", f);
        f = 1 - f;
        printf("new f %d\n", f);
        printf("\n");
    }
    printf("finally costs:\n");
    for (i = 0; i <= n; i++) {
        printf("%lld ", st[f][i].c);
    }
    printf("\n");
    lli min_val = -1;
    for (i = k; i <= n; i++) {
        if (st[f][i].r > 0) {
            if (st[f][i].c >= 0 && (min_val < 0 || st[f][i].c < min_val)) {
                min_val = st[f][i].c;
            }
        }
    }
    printf("%lld\n", min_val);
    return 0;
}
