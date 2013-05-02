#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define ZERO(p, n)      memset(p, 0, n);

/* MOD is prime */
#define MOD 1000000007

typedef long long int lli;

int madd(int a, int b)
{
    return (int)( ((lli)a + (lli)b) % MOD );
}

int mmul(int a, int b)
{
    return (int)( ((lli)a * (lli)b) % MOD );
}

int mpow(int a, int p)
{
    int x = 1;
    for (; p; p >>= 1) {
        if (p & 1) {
            x = mmul(x, a);
        }
        a = mmul(a, a);
    }
    return x;
}

int ipow(int a, int p)
{
    int x = 1;
    for (; p; p >>= 1) {
        if (p & 1) {
            x *= a;
        }
        a *= a;
    }
    return x;
}

void ext_euclid(int a, int b, int *gcd, int *x_out, int *y_out)
{
    int x = 0, y = 1;
    int u = 1, v = 0;
    while (a != 0) {
        div_t qr = div(b, a);
        int m = x - u * qr.quot;
        int n = y - v * qr.quot;
        b = a;
        a = qr.rem;
        x = u, y = v;
        u = m, v = n;
    }
    *gcd = b;
    if (x_out) {
        *x_out = x;
    }
    if (y_out) {
        *y_out = y;
    }
}

/* Revert by modulo with 2 ways:
    1) ext_euclid(x, MOD, &gcd, &inv, 0);
    2) inv = mpow(x, MOD - 2), by Euler theorem, phi(MOD) = MOD - 2
*/

int main()
{
    return 0;
}
