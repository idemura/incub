#include <algorithm>
#include <map>
#include <vector>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define INF 0x7fffffff

typedef long long int lli;

// MOD is prime
const int MOD = 1000000007;

// Logarithm 2: n >= 2 ^ ilog2f(n). Undefined for 0.
int ilog2f(int n)
{
    int i;
    for (i = 0; n != 1; i++) {
        n >>= 1;
    }
    return i;
}

// Logarithm 2: n <= 2 ^ ilog2f(n). Undefined for 0.
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

bool is_prime(int n)
{
    if (n <= 5) {
        return n != 4;
    }
    if (n % 2 == 0 || n % 3 == 0) {
        return false;
    }
    int f = 5;  // Factor.
    int c = 2;
    div_t qr = div(n, f);
    while (qr.rem != 0 && qr.quot > f) {
        f += c = 6 - c;
        qr = div(n, f);
    }
    return qr.rem != 0;
}

// Revert by modulo with 2 ways:
//  1) ext_euclid(x, MOD, &gcd, &inv, 0);
//  2) inv = mpow(x, MOD - 2), by Euler theorem, phi(MOD) = MOD - 1

// Inverts all numbers in [0..mod) where `mod` is prime into `inv` (should be
// at least `mod` long).
void inv_list(int *inv, int mod)
{
    inv[0] = 0;
    inv[1] = 1;
    for (int i = 2; i < mod; i++) {
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
    int *pas = new int[(2 + n) * (n + 1) / 2]();
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

void sieve(int n, std::vector<int> *primes)
{
  char *seq = new char[n + 1]();
  // +1 for case of precise square of large magnitude. Don't sure if it's
  // actually needed.
  int sqrtn = (int)sqrt(n + 1);
  int i;
  for (i = 2; i <= sqrtn; i++) {
    if (seq[i]) {
      continue;
    }
    for (int j = i * i; j <= n; j += i) {
      seq[j] = 1;
    }
    primes->push_back(i);
  }
  for (; i <= n; i++) {
    if (!seq[i]) {
      primes->push_back(i);
    }
  }
  delete[] seq;
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
