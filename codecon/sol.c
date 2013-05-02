#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define ZERO(p, n)      memset(p, 0, n);

int find_max(int *m, int n, int *d, int i0, int j0)
{
    int i, j;
    int max_sum = -1000000000;
    ZERO(d, n * n * sizeof(*d));
    for (i = i0; i < n; ++i) {
        for (j = j0; j < n; ++j) {
            int fi = i * n + j; /* flat index */
            int sum1 = i > i0? d[fi - n]: 0;
            int sum2 = j > j0? d[fi - 1]: 0;
            int sum3 = i > i0 && j > j0? d[fi - n - 1]: 0;
            int sum = sum1 + sum2 - sum3 + m[fi];
            if (sum > max_sum) {
                max_sum = sum;
            }
            d[fi] = sum;
        }
    }
    return max_sum;
}

int main(int argc, char **argv)
{
    int n = 0, i, j;
    int *d, *m;
    int max_sum = -1000000000;

    scanf("%d", &n);
    m = malloc(n * n * sizeof(*m));
    for (i = 0; i < n * n; ++i) {
        scanf("%d", &m[i]);
    }

    d = malloc(n * n * sizeof(*d));
    for (i = 0; i < n; ++i) {
        for (j = 0; j < n; ++j) {
            int sum = find_max(m, n, d, i, j);
            if (sum > max_sum) {
                max_sum = sum;
            }
        }
    }
    free(m);
    free(d);

    printf("%d\n", max_sum);
    return 0;
}
