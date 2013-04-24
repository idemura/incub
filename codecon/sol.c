#define _GNU_SOURCE
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <search.h>
#include <stdio.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))

typedef void *rbtree_t;
typedef long long int lli;

int str_cmpf(const void *p1, const void *p2) {
    return strcmp((const char*)p1, (const char*)p2);
}
int int_cmpf(const void *p1, const void *p2) {
    return (long)p1 - (long)p2;
}
void no_free(void *p) {
}

int n_case;

int next_peak(int *vs, int n, int i)
{
    int peak = i;
    for (int j = i + 1; j < n; ++j) {
        if (vs[j] > vs[peak]) {
            peak = j;
        }
    }
    return peak == i? n: peak;
}

int mini(int a, int b)
{
    return a < b? a: b;
}

void solve(int e, int r, int *vs, int n)
{
    printf("e %d r %d\n", e, r);
    int ei = e;
    r = mini(e, r);
    int gain = 0;
    for (int i = 0; i < n; ++i) {
        int peak = next_peak(vs, n, i);
        printf("%i peak %d\n", i, peak);
        if (peak == n) {
            printf("spend all %d\n", ei);
            gain += vs[i] * ei;
            ei = r;
            continue;
        }
        int er = peak == i? e: (peak - i) * r + ei;
        printf("er %d ei %d\n", er, ei);
        if (er >= ei) {
            printf("peak too far\n");
            int can_spend = er - ei;
            if (can_spend > e) {
                can_spend = e;
            }
            printf("can spend %d\n", can_spend);
            gain += vs[i] * can_spend;
            printf("gain +%d\n", vs[i] * can_spend);
            ei -= can_spend;
            printf("left energy %d\n", ei);
        }
        printf("accum energy %d+%d -> %d\n", ei, r, ei + r);
        ei += r;
    }

    printf("\n");
    printf("Case #%d: %d\n", n_case, gain);
}

int main(int argc, char **argv)
{
    int T = 0;
    scanf("%d", &T);

    for (int i = 0; i < T; ++i) {
        int e, r, n;
        scanf(" %d %d %d", &e, &r, &n);
        int *vs = malloc(n * sizeof(*vs));
        for (int j = 0; j < n; ++j) {
            scanf(" %d", &vs[j]);
        }
        n_case = i + 1;
        solve(e, r, vs, n);
        free(vs);
    }
    return 0;
}
