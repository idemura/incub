#include <algorithm>
#include <assert.h>
#include <map>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <utility>
#include <vector>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define INF 0x7fffffff

typedef long long int lli;

bool equal(const char *s, const char *n) {
    int i = 0;
    for (; n[i] && s[i] == n[i]; i++) {
    }
    return n[i] == 0;
}

int powMod(int x, int p, int m) {
    int r = 1;
    for (; p != 0; p >>= 1) {
        if (p & 1) {
            r = r * x % m;
        }
        x = x * x % m;
    }
    return r;
}

int rollingHash(const char *s, int n, int B, int M) {
    int h = 0;
    for (int i = 0; i < n; i++) {
        h = (h * B + s[i]) % M;
    }
    return h;
}

int rabinKarpSearch(const char *s, const char *n) {
    const int s_len = strlen(s);
    const int n_len = strlen(n);
    if (n_len > s_len) {
        return -1;
    }
    // We have B=128 since assume ASCII and singed char.
    const int M = 30103; // Prime.
    const int Bpow = powMod(128, n_len - 1, M);
    int n_hash = 0, s_hash = 0;
    for (int i = 0; i < n_len; i++) {
        n_hash = ((n_hash << 7) + n[i]) % M;
        s_hash = ((s_hash << 7) + s[i]) % M;
    }
    assert(n_hash == rollingHash(n, n_len, 128, M));
    assert(s_hash == rollingHash(s, n_len, 128, M));
    for (int i = 0; i <= s_len - n_len; i++) {
        if (s_hash == n_hash && equal(s + i, n)) {
            return i;
        }
        // Update hash of the string s: subtract `s[i] * B^M-1`. Because of
        // modular arithmetics, make this negative value into positive like
        // this: `x += M`, where x in (-M, 0].
        int sub = M - s[i] * Bpow % M;
        s_hash = (((s_hash + sub) << 7) + s[i + n_len]) % M;
        // assert(s_hash == rollingHash(s + i + 1, n_len, 128, M));
    }
    return -1;
}

int strStrSearch(const char *s, const char *n) {
    const char *p = strstr(s, n);
    return p ? p - s : -1;
}

void test(const char *s, const char *n) {
    int i1 = rabinKarpSearch(s, n);
    int i2 = strStrSearch(s, n);
    printf("Search %s in %s\n", n, s);
    printf("  Rabin-Karp: %d check: %d\n", i1, i2);
    if (i1 != i2) {
        printf("FAILED\n");
    }
}

int main(int argc, char **argv) {
    // #ifndef ONLINE_JUDGE
    //   freopen("in", "r", stdin);
    // #endif
    test("abcabcabaad", "abc");
    test("abcabcabaad", "abca");
    test("abcabcabaad", "cab");
    test("abcabcabaad", "bcab");
    test("abcabcabaad", "aad");
    test("abcabcabaad", "add");
    return 0;
}
