#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define ZERO(p, n)      memset(p, 0, n);

/* MOD is prime */
#define MOD 1000000007

typedef long long int lli;

int mini(int a, int b)
{
    return a < b ? a : b;
}

int maxi(int a, int b)
{
    return a > b ? a : b;
}

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

int gcd(int a, int b) {
    while (b != 0) {
        int t = a % b;
        a = b;
        b = t;
    }
    return a;
}

void egcd(int a, int b, int *gcd, int *x_out, int *y_out)
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

int egcd_rec(int a, int b, int *xa, int *xb)
{
    if (a == 0) {
        *xb = 1;
        *xa = 0;
        return b;
    }
    int q = b / a;
    int r = b % a;
    int ya, yb;
    int gcd = egcd_rec(r, a, &ya, &yb);
    *xa = yb - q * ya;
    *xb = ya;
    return gcd;
}

int is_prime(int n)
{
    if (n <= 2) {
        return 1;
    }
    int f = 2;
    div_t qr = div(n, f);
    while (qr.rem != 0 && qr.quot > f) {
        f++;
        qr = div(n, f);
    }
    return qr.rem != 0;
}

/* Revert by modulo with 2 ways:
    1) ext_euclid(x, MOD, &gcd, &inv, 0);
    2) inv = mpow(x, MOD - 2), by Euler theorem, phi(MOD) = MOD - 1
*/

// Pascal triangle for [0 .. n]. Memory should be freed by `pascal_free`.
int **pascal(int n)
{
    int **pas = malloc((n + 1) * sizeof *pas);
    pas[0] = malloc((2 + n) * (n + 1) / 2 * sizeof **pas);
    int l = 0;
    pas[l][0] = 1;
    int i, j;
    for (i = 1; i <= n; ++i) {
        pas[l + 1] = pas[l] + i;
        pas[l + 1][0] = 1;
        for (j = 1; j < i; ++j) {
            pas[l + 1][j] = pas[l][j - 1] + pas[l][j];
        }
        pas[l + 1][j] = 1;
        l++;
    }
    return pas;
}

void pascal_free(int **pas)
{
    if (pas) {
        free(pas[0]);
        free(pas);
    }
}

int main()
{
    return 0;
}
