#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <limits.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define ZERO(p, n)      memset(p, 0, n);

typedef long long int lli;

int mini(int a, int b)
{
    return a < b ? a : b;
}

int maxi(int a, int b)
{
    return a > b ? a : b;
}

// Pascal triangle for [0 .. n]. Memory should be freed by `pascal_free`.
int **pascal(int n)
{
    return a > b ? a : b;
}

int sq[10002];

typedef struct ilist {
    struct ilist *prev;
    int l, r;
} ilist;

int stupid_rec(int n, int k, ilist *p)
{
    int i;

    if (k == 0) {
        return 1;
    }
    if (n == 0) {
        return 0;
    }
    ilist item = { .prev = p };
    int s = 0;
    k--;
    for (i = 0; i <= k; i++) {
        item.l = i;
        item.r = k - i;
        s += stupid_rec(n - 1, i, &item) * stupid_rec(n - 1, k - i, &item);
    }
    return s;
}

int main()
{
    freopen("in", "r", stdin);

    // int i;
    int n = 0, sm_c = 1;
    // int sm[8], stat[12] = {};
    scanf("%d%d", &n, &sm_c);
    // if (sm_c > ARRAY_SIZEOF(sm) || n >= ARRAY_SIZEOF(stat)) {
    //     return -1;
    // }
    // stupid_sums(n, sm_c, 0, sm, stat);
    int res = stupid_rec(n, sm_c, 0);
    printf("%d\n", res);
    // for (i = 0; i <= n; i++) {
    //     printf("%d - %d\n", i, stat[i]);
    // }
    return 0;
}
