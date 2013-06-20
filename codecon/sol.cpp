#include <algorithm>
#include <vector>
#include <utility>
#include <cstdio>

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
status st[MAXN+1];

bool left_less(const worker& j1, const worker& j2)
{
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
    for (j = 0; j <= n; j++) {
        st[j].c = -1;
        st[j].i = -1;
    }
    sort(ws, ws + m, left_less);
    st[0].c = 0;
    for (i = 0; i < m; i++) {
        for (j = 0; j <= n; j++) {
            if (j != 0 && (st[j].r == 0 || st[j].i == i)) {
                continue;
            }
            if (ws[i].r <= st[j].r) {
                continue;
            }
            int l = max(ws[i].l, st[j].r + 1);
            int r = max(ws[i].r, st[j].r + 1);
            int cov = j + r - l + 1;
            lli val = st[j].c + ws[i].c;
            if (st[cov].c < 0 || val < st[cov].c) {
                st[cov].c = val;
                st[cov].r = ws[i].r;
                st[cov].i = i;
            }
        }
    }
    lli min_val = -1;
    for (i = k; i <= n; i++) {
        if (st[i].c >= 0 && (min_val < 0 || st[i].c < min_val)) {
            min_val = st[i].c;
        }
    }
    printf("%I64d\n", min_val);
    return 0;
}
