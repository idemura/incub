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
            if (b - sq[j - 1] > a) {
                break;
            }
        }
    }
    return c;
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
