#include <algorithm>
#include <vector>
#include <utility>
#include <stdio.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))

using namespace std;

typedef long long int lli;

struct job {
    int l, r, c;
};

#define MAXN 300

job js[100000];
int rb[MAXN+1];
lli cost[MAXN+1];

bool left_less(const job& j1, const job& j2) {
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
    const lli HUGE = 0x7fffffffffffffffll;
    int i, j, k, n, m;
    scanf("%d%d%d", &n, &m, &k);
    for (i = 0; i < m; i++) {
        scanf("%d%d%d", &js[i].l, &js[i].r, &js[i].c);
    }
    for (j = 0; j <= n; j++) {
        cost[j] = HUGE;
    }
    sort(js, js + m, left_less);
    for (i = 0; i < m; i++) {
        printf("%d-%d cost %d\n", js[i].l, js[i].r, js[i].c);
    }
    rb[0] = 1;
    cost[0] = 0;
    for (i = 0; i < m; i++) {
        printf("job %d-%d cost %d\n", js[i].l, js[i].r, js[i].c);
        for (j = 0; j <= n; j++) {
            printf("j %d rb[j] %d\n", j, rb[j]);
            if (rb[j] <= 0) {
                printf("skip\n");
                continue;
            }
            int cl = max(js[i].l, rb[j]);
            int cr = max(js[i].r, rb[j]);
            printf("cl %d\n", cl);
            printf("cr %d\n", cr);
            int new_cov = j + cr - cl;
            printf("new_cov %d\n", new_cov);
            lli new_val = cost[j] + js[i].c;
            if (new_val < cost[new_cov]) {
                printf("update %d:\n", new_cov);
                cost[new_cov] = new_val;
                rb[new_cov] = js[i].r;
                printf("cost %lld\n", new_val);
                printf("rb %d\n", js[i].r);
            }
        }
        printf("\n");
    }
    printf("finally costs:\n");
    for (i = 0; i <= n; i++) {
        printf("%lld ", cost[i]);
    }
    printf("\n");
    lli min_val = HUGE;
    for (i = k; i <= n; i++) {
        if (rb[i] > 0) {
            if (cost[i] < min_val) {
                min_val = cost[i];
            }
        }
    }
    if (min_val != HUGE) {
        printf("%lld\n", min_val);
    } else {
        printf("-1\n");
    }
    return 0;
}
