#include <algorithm>
#include <assert.h>
#include <ctype.h>
#include <map>
#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <sys/time.h>
#include <utility>
#include <vector>

#define ARRAY_SIZEOF(A) (sizeof(A) / sizeof(A[0]))
#define INF 0x7fffffff
#define NON_COPYABLE(C)                                                        \
    C(const C &);                                                              \
    C &operator=(const C &);

typedef long long int lli;

inline int madd(int a, int b, int p) {
    return (a + b) % p;
}

inline int msub(int a, int b, int p) {
    return (a - b + p) % p;
}

inline int mmul(lli a, int b, int p) {
    return (int)(a * b % p);
}

int mpow(lli a, int k, int p) {
    lli x = 1;
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

void extGCD(int a, int b, int *gcd, int *x_out, int *y_out) {
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
    *x_out = x;
    *y_out = y;
}

inline int minv(int a, int p) {
    int gcd, x, y;
    extGCD(a, p, &gcd, &x, &y);
    return x < 0 ? x + p : x;
}

inline int mdiv(lli a, int b, int p) {
    a *= minv(b, p);
    a %= p;
    return (int)a;
}

inline bool eurlerCriterion(int a, int p) {
    return mpow(a, (p - 1) / 2, p) == 1;
}

int findQuadraticNonResidue(int p) {
    // Brute force search for the first non-residue.
    int k = 2;
    for (; eurlerCriterion(k, p); k++) {
    }
    return k;
}

// Returns a square root on `a` modulo `p` or -1 if doesn't exist.
int shanksTonelli(int a, int p) {
    if (p == 2 || a <= 1) {
        return a;
    }
    if (!eurlerCriterion(a, p)) {
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
    int g = mpow(findQuadraticNonResidue(p), s, p);
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

void solve(int a, int b, int c, int p, std::vector<int> &v) {
    if (a == 0) {
        v.push_back(mdiv(p - c, b, p));
    } else if (c == 0) {
        v.push_back(0);
        if (b != 0) {
            v.push_back(mdiv(p - b, a, p));
        }
    } else if (b == 0) {
        int x = shanksTonelli(p - mdiv(c, a, p), p);
        if (x < 0) {
            return;
        }
        v.push_back(x);
        v.push_back(p - x);
    } else {
        int t = minv(2 * a, p);
        lli f = mmul(b, t, p);
        lli D = (f * f - 2ll * t * c) % p;
        if (D < 0) D += p;
        int Dsqrt1 = shanksTonelli((int)D, p);
        if (Dsqrt1 < 0) {
            return;
        }
        int Dsqrt2 = p - Dsqrt1;
        int x1 = (Dsqrt1 - f + p) % p;
        int x2 = (Dsqrt2 - f + p) % p;
        v.push_back(x1);
        if (x1 != x2) {
            v.push_back(x2);
        }
    }
}

template <class T>
inline void swap_gt(T &a, T &b) {
    if (a > b) std::swap(a, b);
}

int main(int argc, char **argv) {
#ifndef ONLINE_JUDGE
    freopen("in", "r", stdin);
#endif
    int T = 0;
    scanf("%d", &T);
    std::vector<int> v;
    for (int i = 0; i < T; i++) {
        int a, b, c, p;
        scanf("%d%d%d%d", &a, &b, &c, &p);
        v.clear();
        solve(a, b, c, p, v);
        if (v.size() == 2) {
            swap_gt(v[0], v[1]);
        }
        printf("%zu ", v.size());
        for (auto x : v) {
            printf("%d ", x);
        }
        printf("\n");
    }
    return 0;
}
