#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define ZERO(p, n)      memset(p, 0, n);

typedef long long int lli;

typedef struct {
    char str[8];
    int l, c;
    int bit_c;
} ip;

int get_mask(int *digit_mask, int n)
{
    int m = 0;
    int r0 = n % 10;
    n /= 10;
    int r1 = n % 10;
    n /= 10;
    int r2 = n % 10;
    return
    if (!digit_mask[r])
        return 0;
    m |= digit_mask[r];
    if (n < 10)
        return m;
    // No need to div by 10
    r = n % 10;
    if (!digit_mask[r])
        return 0;
    m |= digit_mask[r];
    return m;
}

int main(void)
{
    int i, j, n, mask[10] = {};
    ip ips[65536];

    scanf("%d", &n);
    for (i = 0; i < n; i++) {
        int d;
        scanf("%d", &d);
        mask[i] = 1 << d;
    }

    int c = 0;
    for (i = 0; i < 256; i++) {
        int m = get_mask(digit_mask, i);
        if (!m) {
            continue;
        }
        printf("%d is OK.\n", i);
    }

    return 0;
}
