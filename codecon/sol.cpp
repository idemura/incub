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
    int i;
    lli c;
};

#define MAXN 300

worker ws[100000];
status st[MAXN+1][MAXN+1];

bool left_less(const worker& j1, const worker& j2)
{
    if (j1.l == j2.l)
        return j1.r < j2.r;
    else
        return j1.l < j2.l;
}

void print_state(int n)
{
    printf("status:\n");
    for (int i = 0; i <= n; i++) {
        printf("%d/%lld ", st[i].r, st[i].c);
    }
    printf("\n");
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
    for (j = 0; j <= n; j++) {
        st[j].c = -1;
        st[j].i = -1;
    }
    sort(ws, ws + m, left_less);
    for (i = 0; i < m; i++) {
        printf("%d-%d cost %d\n", ws[i].l, ws[i].r, ws[i].c);
    }
    // every 0-length coverage cost 0 regardless of their right bounds.
    for (i = 0; i <= n; i++) {
        st[0][i].c = 0;
    }
    // it's might be enough to just init this one element?
    // st[0].c = 0;
    printf("m %d\n", m);
    // the idea is to coverage and right bound (so this is
    // a table with 2 indices).
    for (i = 0; i < m; i++) {
        printf("worker %d-%d cost %d\n", ws[i].l, ws[i].r, ws[i].c);
        print_state(n);

        for (j = 0; j <= n; j++) {
            if (j != 0 && (st[j].r == 0 || st[j].i == i)) {
                // printf("skip\n");
                continue;
            }
            printf("j %d r[j] %d\n", j, st[j].r);
            if (ws[i].r <= st[j].r) {
                continue;
            }
            int l = max(ws[i].l, st[j].r + 1);
            int r = max(ws[i].r, st[j].r + 1);
            printf("l %d r %d\n", l, r);
            int cov = j + r - l + 1;
            lli val = st[j].c + ws[i].c;
            printf("cov %d val %lld\n", cov, val);
            if (st[cov].c < 0 || val < st[cov].c) {
                printf("update %d:\n", cov);
                st[cov].c = val;
                st[cov].r = ws[i].r;
                st[cov].i = i;
                printf("  cost %lld\n", val);
                printf("  r %d\n", ws[i].r);
            }
        }
        print_state(n);
        printf("\n");
    }
    printf("finally\n");
    print_state(n);
    lli min_val = -1;
    for (i = k; i <= n; i++) {
        if (st[i].c >= 0 && (min_val < 0 || st[i].c < min_val)) {
            min_val = st[i].c;
        }
    }
    printf("%lld\n", min_val);
    return 0;
}
