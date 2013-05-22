#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define ZERO(p, n)      memset(p, 0, n);

typedef long long int lli;

int gcd(int a, int b) {
    while (b != 0) {
        int t = a % b;
        a = b;
        b = t;
    }
    return a;
}

int count_py(int n)
{
    int i, j;
    int c = 0;
    for (i = 1; i <= n; i++) {
        for (j = i + 1; j <= n; ++j) {
            int r = i * i + j * j;
            if (r > n)
                break;
            if (gcd(i, j) == 1 && ((j - i) & 1) == 1) {
                c += n / r;
            }
        }
    }
    return c;
}

int main(int argc, char **argv)
{
    // freopen("in", "r", stdin);
    int n = 0;
    scanf("%d", &n);
    printf("%d\n", count_py(n));
    return 0;
}
