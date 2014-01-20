#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

#define ARRAY_SIZEOF(a)     (sizeof(a) / sizeof(a[0]))
#define ARRAY_ZERO(p, n)    memset(p, 0, (n) * sizeof(*(p)))

// MOD is prime
#define MOD 1000000007

typedef long long int lli;

// Floor logarithm. Undefined for 0.
int ilog2f(int n)
{
    int i;
    for (i = 0; n != 1; i++) {
        n >>= 1;
    }
    return i;
}

// Ceiling logarithm. Undefined for 0.
int ilog2c(int n)
{
    int i;
    for (i = 0; (1 << i) < n; i++) {
    }
    return i;
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

int minv(int a)
{
    return mpow(a, MOD - 2);
}

int gcd(int a, int b)
{
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

/* Inverts all numbers in [0..mod-1] where mod is prime. Result saved in `inv`
   which is at least `mod` long.
*/
void inv_list(int *inv, int mod)
{
    int i;
    inv[0] = 0;
    inv[1] = 1;
    for (i = 2; i < mod; i++) {
        // Let consider mod (which is prime) and mod % i. Extended Euclid on
        // them gives:
        //  mod * k1 + (mod % i) * k2 = 1
        // Here k2 = inv(mod % i) = r and already know. Put
        //  mod % i = mod - mod / i * i
        // in the first equation and re-group it:
        //  mod * k1 + (mod - mod / i * i) * r = 1
        //  mod * (k2 + r) - (mod / i * r) * i = 1
        // so the result for inv(i):
        //  inv(i) = - (mod / i) * inv(mod % i)
        inv[i] = mod - ((mod / i) * inv[mod % i]) % mod;
    }
}


// Pascal triangle for [0 .. n]. Memory should be freed by `pascal_free`.
int *pascal(int n)
{
    int *pas = new int[(2 + n) * (n + 1) / 2];
    int i, j, in = 0, out = 0;
    pas[out++] = 1;
    for (i = 1; i <= n; ++i) {
        int next_in = out;
        pas[out++] = 1;
        for (j = 1; j < i; ++j) {
            pas[out++] = pas[in] + pas[in + 1];
            in++;
        }
        pas[out++] = 1;
        in = next_in;
    }
    return pas;
}

// Pascal coefficients for degree d, d >= 0.
void pascal_get(int d, int *i0, int *i1)
{
    *i0 = (d + 1) * d / 2;
    *i1 = (d + 1) + *i0;
}

void pascal_free(int *pas)
{
    delete[] pas;
}

/* Takes `list` with size `list_n` as a buffer (no assumptions on values in it)
   and runs Eratosthenes sieve on it. Returns count of primes in list, which
   stored in 0..list_n in the list. Max prime can be found is `list_n + 1`.

   Example of usage:

    int primes[12];
    int primes_n = sieve(primes, ARRAY_SIZEOF(primes));
    // So, max prime found is 13.
*/
int sieve(int *list, int list_n)
{
    ARRAY_ZERO(list, list_n);
    int i, j, w = 0;
    int sqrt_n = (int)sqrt(list_n);
    for (i = 0; i <= sqrt_n; ++i) {
        if (!list[i]) {
            int prime = i + 2;
            /* The first index to go, -2 for index shift (don't 0 and 1 take
               into account).
               Start from square of prime p. Let's assume the opposite: there
               is some composite m = p * j, m < p * p, hence j < p.  Therefore,
               this j should have some prime divisor p' <= j < p and hence,
               m should be rejected in the process for j.
            */
            int first = prime * prime - 2;
            for (j = first; j < list_n; j += prime) {
                list[j] = 1;
            }
        }
    }
    // Copy primes to the beginning.
    for (i = 0; i < list_n; i++) {
        if (!list[i]) {
            list[w++] = i + 2;
        }
    }
    return w;
}

// Function computing the floor of the square root, by Dijkstra
int sqrti(int n)
{
    int p, q, r, h;
    p = 0;
    q = 1;
    r = n;
    while (q <= n) {
        q *= 4;
    }
    while (q != 1) {
        q /= 4;
        h = p + q;
        p /= 2;
        if (r >= h) {
            p += q;
            r -= h;
        }
    }
    return p;
}

int main()
{
    return 0;
}
