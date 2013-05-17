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

int sq[10002];

int square_of(int n, int i0, int i1) {
    while (i0 < i1) {
        int m = (i0 + i1) / 2;
        if (n <= sq[m]) {
            i1 = m;
        } else {
            i0 = m + 1;
        }
    }
    return sq[i0] == n? i0: 0;
}

int count_pythagorian(int n)
{
    int i, j;
    int c = 0;
    for (i = 1; i <= n; i++) {
        int a = sq[i];
        for (j = i + 1; j <= n; j++) {
            int b = sq[j];
            int sq_r = square_of(a + b, j, mini(i + j, n));
            if (sq_r) {
                c++;
                // printf("%d(%d) %d(%d) -> %d(%d - %d)\n", i, a, j, b, sq_r, a + b, sq[sq_r]);
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

int count_0(int **pas, int n)
{
    if (n == 0) {
        return 0; // Not sure, probably 4 is better
    }
    int s = 0;
    s += pas[n - 1][0] * 3;
    if (1 < n) {
        s += pas[n - 1][1] * 2;
        if (2 < n) {
            s += pas[n - 1][2];
        }
    }
    return s;
}

int count_n(int **pas, int n)
{
    if (n == 0) {
        return 1;
    }
    int s = 0;
    s += pas[n - 1][0] * 2;
    if (1 < n) {
        s += pas[n - 1][1] * 3;
        if (2 < n) {
            s += pas[n - 1][2] * 4;
        }
    }
    return s;
}

void init_table(int **pas, int n, int *tab0, int *tabx)
{
    int i;
    for (i = 0; i <= n; i++) {
        tab0[i] = count_0(pas, i);
    }
    for (i = n; i >= 0; i--) {
        tabx[n - i] = i == 0 ? count_0(pas, n)
                             : count_n(pas, n - i);
    }
}

int pascal_sum_n(int **pascal, int n, int nsum)
{
    n = mini(n + 1, nsum);
    int i, sum = 0;
    for (i = 0; i < n; i++) {
        sum += pascal[n][i];
    }
    return n;
}

void print_for(int n, int *tab0, int *tabx)
{
    int i;
    for (i = 0; i <= n; ++i) {
        int c = i == 0? tab0[n] : tabx[n - i];
        printf("%d - %d\n", i, c);
    }
}

void sol_table(int N, int K, int *tab0, int *tabx)
{
#define Rn 30
#define Cn 1001
    int st[Rn][Cn] = {};
    int i, j, k;
    // First index decodes to 2^(i+1)-1
    for (i = 0; i < N; i++) {
        st[i][0] = 1;
    }
    int d = 3;
    for (i = 1; i < N; i++) {
        for (j = 1; j <= K; j++) {
            printf("cover %d in %d steps\n", d, j);
            int s = tab0[j - 1];
            printf("initial s %d\n", s);
            for (k = 0; k <= j - 1; k++) {
                int m = tabx[k]; // Multiplier
                int v = st[i - 1][j - 1 - k];
                printf("m %d v %d\n", m, v);
                s += m * v;
            }
            printf("setting %d\n", s);
            st[i][j] = s;
        }
        d = 2 * d + 1;
    }

    d = 1;
    for (i = 0; i < N; ++i) {
        printf("%d: ", d);
        for (j = 0; j <= K; j++) {
            printf("%d ", st[i][j]);
        }
    }
}

void test_case(int test_n)
{
    int i;
    sums_total = 0;
    memset(counts, 0, sizeof counts);
    printf("test_n %d\n", test_n);
    int **pas = pascal(test_n);

    // int sum_s = pascal_sum_n(pas, test_n - 1, SUMMANDS);
    // printf("Sum %d pascal coefs: %d\n", SUMMANDS, sum_s);

    // int ms[SUMMANDS] = {};
    // // all_sums_of(test_n, ms);
    // sums(test_n, test_n, 0, ms);
    // printf("TOTAL: %d\n", sums_total);
    // printf("num stats:\n");

    // int num_sum = 0;
    // for (i = test_n; i >= 0; i--) {
    //     printf("  %d - %d\n", i, counts[i]);
    //     num_sum += counts[i];
    // }
    // printf("nums sum %d\n", num_sum);

    // printf("zeros by formula %d\n", count_0(pas, test_n));
    for (i = 0; i <= test_n; i++) {
        int c = i == 0 ? count_0(pas, test_n)
                       : count_n(pas, test_n - i);
        printf("#%d %d\n", i, c);
    }

    int tab0[1000+1], tabx[1000+1];
    init_table(pas, test_n, tab0, tabx);
    printf("tab0\n");
    for (i = 0; i <= test_n; i++) {
        printf("%d ", tab0[i]);
    }
    printf("\n");
    printf("tabx\n");
    for (i = 0; i <= test_n; i++) {
        printf("%d ", tabx[i]);
    }
    printf("\n");
    // print_for(test_n, tab0, tabx);
    printf("---------\n");
    sol_table(3, 3, tab0, tabx);
    pascal_free(pas);
}

int main(int argc, char **argv)
{
#ifdef DEBUG
    freopen("in", "r", stdin);
#endif

    int i, n = 0;
    scanf("%d", &n);
    for (i = 0; i < 10002; i++) {
        sq[i] = i * i;
    }
    printf("%d\n", count_pythagorian(n));
    return 0;
}
