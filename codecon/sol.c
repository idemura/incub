#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <limits.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define ZERO(p, n)      memset(p, 0, n);

/* MOD is prime */
#define MOD 7340033

typedef long long int lli;

int mini(int a, int b)
{
    return a < b ? a : b;
}

void printm(int *t, int n, int m)
{
    int i, j;
    int *p = t;
    for (i = 0; i < n; ++i) {
        for (j = 0; j < m; ++j) {
            printf("%2d ", *p++);
        }
        printf("\n");
    }
}

int rec(int *t, int n, int k, int i, int j) {
    if (j < 0) {
        return 0;
    }
    int *tij = &t[i * (k + 1) + j];
    if (*tij >= 0) {
        return *tij;
    }
    lli s = (lli)rec(t, n, k, i - 1, j - 4) +
            (lli)(4 * rec(t, n, k, i - 1, j - 3)) +
            (lli)(6 * rec(t, n, k, i - 1, j - 2)) +
            (lli)(4 * rec(t, n, k, i - 1, j - 1));
    return *tij = s % MOD;
}

int solve(int n, int k)
{
    int i, m, r;
    int *t = 0, tab_size = 0, ans;

    printf("---------------------\n");
    printf("input n %d k %d\n", n, k);
    if (n % 2 == 0) {
        return k == 0;
    }
    if (k == 0) {
        return 1;
    }
    /* Draw first square. */
    n >>= 1;
    if (n == 0) {
        return k == 0;
    }
    k--;
    printf("OK, n and k now: %d %d\n", n, k);

    m = 0; /* `m` is count of left bits set */
    r = n;
    for (; r & 1; r >>= 1) {
        ++m;
    }
    printf("m %d r %d\n", m, r);

    tab_size = m * (k + 1);
    t = malloc(tab_size * sizeof *t);
    for (i = 0; i < tab_size; ++i) {
        t[i] = -1;
    }
    printm(t, m, k + 1);

    for (i = 0; i < m; ++i) {
        t[(k + 1) * i] = 1;
    }
    printf("set col 0:\n");
    printm(t, m, k + 1);
    t[1] = r != 0;
    printf("set t[1]:\n");
    printm(t, m, k + 1);
    for (i = 2; i <= k; ++i) {
        t[i] = 0;
    }
    printf("set row 0:\n");
    printm(t, m, k + 1);
    /*  ok, let's do it this way:
        recursively in function frec n, k:
            if k == 0:
                return 1;
            if n == 1 || n % 2 == 0:
                return 0;
            so, k > 0 and n odd here. We should split:
            k--;
            so possible different combination of how to split new k steps
            into groups of 4, with order difference:
            k 0 0 0
    */

    // for (i = 1; i < m; ++i) {
    //     for (j = 1; j <= k; ++j) {
    //         int ij = (i - 1) * (k + 1) + j;
    //         lli sum = 4 * t[ij - 1];
    //         if (j >= 2) {
    //             sum += 6 * t[ij - 2];
    //             if (j >= 3) {
    //                 sum += 4 * t[ij - 3];
    //                 if (j >= 4) {
    //                     sum += t[ij - 4];
    //                 }
    //             }
    //         }
    //         t[i * (k + 1) + j] = sum % MOD;
    //         printf("set %i %i\n", i, j);
    //         printm(t, m, k + 1);
    //     }
    // }
    // ans = t[(m - 1) * (k + 1) + k];
    ans = rec(t, m, k, m - 1, k);
    printf("sol matrix:\n");
    printm(t, m, k + 1);
    printf("ans %d\n", ans);
    free(t);
    return ans;
}

int f(int k, int g)
{
    if (g == 1) {
        return 1;
    }
    int s = 0;
    int i;
    for (i = 0; i <= k; ++i) {
        s += f(k - i, g - 1);
    }
    return s;
}

int* make_ftab(int k, int g)
{
    int i, j;
    int *tab = malloc((k + 1) * g * sizeof *tab);
    for (i = 0; i <= k; ++i) {
        tab[i] = 1;
    }
    for (j = 1; j < g; ++j) {
        int s = 0;
        for (i = 0; i <= k; ++i) {
            int ij = (k + 1) * j + i;
            s += tab[ij - (k + 1)];
            tab[ij] = s;
        }
    }
    for (j = 0; j < g; ++j) {
        for (i = 0; i <= k; ++i) {
            printf("%2d ", tab[(k + 1) * j + i]);
        }
        printf("\n");
    }
    return tab;
}

void free_ftab(int *tab)
{
    free(tab);
}

void all_sums_of(int n, int *ms, int *ms_last)
{
    int i;
    if (n == 0) {
        if (ms_last[0] != ms[0] ||
                ms_last[1] != ms[1] ||
                ms_last[2] != ms[2] ||
                ms_last[3] != ms[3]) {
            printf("%d %d %d %d\n", ms[0], ms[1], ms[2], ms[3]);
            ms_last[0] = ms[0];
            ms_last[1] = ms[1];
            ms_last[2] = ms[2];
            ms_last[3] = ms[3];
        }
        return;
    }
    for (i = 0; i < 4; ++i) {
        if (i == 0 || ms[i - 1] >= ms[i] + 1) {
            ms[i]++;
            all_sums_of(n - 1, ms, ms_last);
            ms[i]--;
        }
    }
}

#define SUMMANDS 4

static counts[120];
static int sums_total = 0;

void sums(int n, int nmax, int j, int *ms)
{
    int i, k;
    // printf("%d%d\n", 4 - j, n);
    // printf("rec params n %d nmax %d j %d ms %d %d %d %d\n", n, nmax, j,
    //         ms[0], ms[1], ms[2], ms[3]);

    // if (n < 0) {
    //     printf("n is negative, %d\n", n);
    //     return;
    // }
    if (j == SUMMANDS || n == 0) {
        for (i = j; i < SUMMANDS; ++i) {
            ms[i] = 0;
        }
        if (n == 0) {
            printf("%d %d %d %d\n", ms[0], ms[1], ms[2], ms[3]);
            for (i = 0; i < SUMMANDS; ++i) {
                counts[ms[i]]++;
            }
            sums_total++;
        }
        return;
    }
    // int from = mini(n, nmax);
    int from = n;
    for (k = from; k > 0; --k) {
        // printf("j %d - k %d\n", j, k);
        ms[j] = k;
        sums(n - k, k, j + 1, ms);
    }
}

int pascal_tri(int n)
{
    int *l[2] = {
        malloc(2 * (n + 1) * sizeof l[0][0]),
    };
    l[1] = l[0] + n + 1;
    l[0][0] = 1;
    int i, j, ri = 0;
    for (i = 1; i <= n; ++i) {
        int wi = 1 - ri;
        l[wi][0] = 1;
        for (j = 1; j < i; ++j) {
            l[wi][j] = l[ri][j - 1] + l[ri][j];
        }
        l[wi][j] = 1;
        ri = wi;
        // printf("%d: ", i);
        // for (j = 0; j <= i; ++j) {
        //     printf("%d ", l[ri][j]);
        // }
        // printf("\n");
    }
    int first_s = mini(SUMMANDS, n);
    int sum = 0;
    for (i = 0; i < first_s; ++i) {
        sum += l[ri][i];
    }
    return sum;
}

void test_case(int test_n)
{
    int i;
    sums_total = 0;
    memset(counts, 0, sizeof counts);
    printf("test_n %d\n", test_n);
    int sum_s = pascal_tri(test_n - 1);
    printf("Sum %d pascal coefs: %d\n", SUMMANDS, sum_s);
    int ms[SUMMANDS] = {};
    // printf("init:\n%d %d %d %d\n", ms[0], ms[1], ms[2], ms[3]);
    // all_sums_of(test_n, ms);
    sums(test_n, test_n, 0, ms);
    printf("TOTAL: %d\n", sums_total);
    printf("num stats:\n");
    for (i = test_n; i >= 0; i--) {
        printf("  %d - %d\n", i, counts[i]);
    }
}

int main(int argc, char **argv)
{
    int i;
    if (argc >= 2) {
        int n0 = atoi(argv[1]);
        int n1 = n0;
        if (argc >= 3) {
            n1 = atoi(argv[2]);
        }
        for (i = n0; i <= n1; i++) {
            test_case(i);
        }
    }
    return 0;

    printf("%d\n", f(4, 3));
    printf("%d\n", f(3, 2));
    int *tab = make_ftab(4, 3);
    free_ftab(tab);
    return 0;

    int T, N, K;

    scanf("%d", &T);
    for (i = 0; i < T; ++i) {
        scanf("%d%d", &N, &K);
        printf("%d\n", solve(N, K));
    }
    return 0;
}
