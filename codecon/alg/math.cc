#include "base.h"

struct ExtGCD {
    int gcd, a, b;
    ExtGCD(int gcd, int a, int b): gcd(gcd), a(a), b(b) {}
};

ExtGCD ext_gcd(int a, int b) {
    if (b == 0) {
        return {a, 1, 0};
    }
    auto r = ext_gcd(b, a % b);
    return {r.gcd, r.b, r.a - (a / b) * r.b};
}

int madd(int a, int b, int p) {
    return (a + b) % p;
}

int msub(int a, int b, int p) {
    return (a - b + p) % p;
}

int mmul(i64 a, int b, int p) {
    return (int)(a * b % p);
}

int mpow(i64 a, int k, int p) {
    i64 x = 1;
    for (; k; k >>= 1) {
        if (k & 1) {
            x *= a;
            x %= p;
        }
        a *= a;
        a %= p;
    }
    return (int)x;
}

int minv(int a, int p) {
    return mpow(a, p - 2, p);
}

// Compared to pow version, this may invert if `p` if not prime; but it has to
// be coprime with `a`.
int minv_bezout(int a, int p) {
    auto g = ext_gcd(a, p);
    if (g.gcd != 1)
        return 0; // Inverse doesn't exist.
    else
        return x < 0 ? x + p : x;
}

int mdiv(i64 a, int b, int p) {
    a *= minv(b, p);
    a %= p;
    return (int)a;
}

bool eurler_criterion(int a, int p) {
    return mpow(a, (p - 1) / 2, p) == 1;
}

int find_quadratic_non_residue(int p) {
    // Brute force search for the first non-residue.
    int k = 2;
    while (eurler_criterion(k, p)) {
        k++;
    }
    return k;
}

// Returns a square root on `a` modulo `p` or -1 if doesn't exist.
// Shanks-Toneilli algorithm, see
// http://www.math.vt.edu/people/brown/doc/sqrts.pdf
int shanks_tonelli(int a, int p) {
    if (p == 2 || a <= 1) {
        return a;
    }
    if (!eurler_criterion(a, p)) {
        return -1; // No square root.
    }
    if (p % 4 == 3) {
        return mpow(a, (p + 1) / 4, p);
    }

    int s = p - 1, e = 0;
    for (; (s & 1) == 0; s /= 2) {
        e++;
    }

    int x = mpow(a, (s + 1) / 2, p);
    int b = mpow(a, s, p);
    int g = mpow(find_quadratic_non_residue(p), s, p);
    int r = e;
    for (;;) {
        int m = 0;
        for (int y = b; y != 1; y = mmul(y, y, p)) {
            m++;
        }
        if (m == 0) {
            return x;
        }
        int g1 = mpow(g, 1 << (r - m - 1), p);
        x = mmul(x, g1, p);
        int g2 = mmul(g1, g1, p);
        b = mmul(b, g2, p);
        g = g2;
        r = m;
    }
    return -1;
}

bool is_prime(int n) {
    if (n <= 5) {
        return n > 1 && n != 4;
    }
    if (n % 2 == 0 || n % 3 == 0) {
        return false;
    }
    int f = 5; // Factor.
    int c = 4;
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
//
// Inverts all numbers in [0..mod) where `mod` is prime into `inv` (should be
// at least `mod` long).
//
// Apply extended Euclid on `mod` (which is prime) and `mod % i`:
//  [1] mod * k1 + (mod % i) * k2 = 1
// `k2 = inv(mod % i)` is already known, because `mod % i < i`. Substitute in
// [1] `mod % i = mod - mod / i * i`:
//  mod * k1 + (mod - mod / i * i) * k2 = 1
//  mod * (k1 + k2) - (mod / i * r) * i = 1
// In the latest equation we can spot extended Euclid equation with, hence:
//  inv(i) = - (mod / i) * inv(mod % i)
vector<int> invert_all(int mod) {
    vector<int> inv(mod);
    inv[0] = 0;
    inv[1] = 1;
    for (int i = 2; i < mod; i++) {
        inv[i] = mod - (i64(mod / i) * inv[mod % i]) % mod;
    }
    return move(inv);
}

// Function computing the floor of the square root, by Dijkstra
int sqrti(int n) {
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

// Input: 32 bit unsigned integer.
int count_bits32(unsigned int n) {
    // Treat `n` as sums of groups of 1 bit.
    n = (n & 0x55555555u) + ((n & 0xaaaaaaaau) >> 1);
    // `n` is now sum of groups of 2 bits and so on on next steps.
    n = (n & 0x33333333u) + ((n & 0xccccccccu) >> 2);
    n = (n & 0x0f0f0f0fu) + ((n & 0xf0f0f0f0u) >> 4);
    n = (n & 0x00ff00ffu) + ((n & 0xff00ff00u) >> 8);
    n = (n & 0x0000ffffu) + (n >> 16);
    return (int)n;
}

int main() {
    return 0;
}
