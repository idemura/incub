#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define ZERO(p, n)      memset(p, 0, n)

typedef long long int lli;

#define NMAX 1000000

int n, k, ns[5000], diffs[NMAX], sieve[NMAX+10];

int int_less(const void *p1, const void *p2)
{
    return *(int*)p1 - *(int*)p2;
}

int main(void)
{
    // 3 2
    // 120 180 300
#ifndef ONLINE_JUDGE
    freopen("in", "r", stdin);
#endif
    int i, j, nmax = 0;

    scanf("%d%d", &n, &k);
    for (i = 0; i < n; i++) {
        scanf("%d", &ns[i]);
        if (ns[i] > nmax) {
            nmax = ns[i];
        }
    }

    qsort(ns, n, sizeof *ns, int_less);

    for (i = 0; i < n; i++) {
        for (j = i + 1; j < n; j++) {
            int d = ns[j] - ns[i];
            diffs[d]++;
        }
    }

    for (i = 2; i <= nmax + 1; i++) {
        int c = diffs[i];
        for (j = i + i; j <= nmax + 1; j += i) {
            sieve[j]++;
            c += diffs[j];
        }
        if (c <= k) {
            break;
        }
    }

    printf("%d\n", i);
    return 0;
}
