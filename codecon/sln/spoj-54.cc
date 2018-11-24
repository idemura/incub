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

struct BigNum {
    char s[102];
    int sn;

    BigNum(): s(), sn() {}
    void revert() {
        for (int i = 0, j = sn - 1; i < j; i++, j--) {
            char t = s[i];
            s[i] = s[j];
            s[j] = t;
        }
    }

    void toInts() {
        for (int i = 0; i < sn; i++) {
            s[i] -= '0';
        }
    }

    void toChars() {
        for (int i = 0; i < sn; i++) {
            s[i] += '0';
        }
    }

    void trimZeroes() {
        for (; s[sn - 1] == 0; sn--) {
        }
    }
};

// a -= b;
void subtract(BigNum *a, BigNum *b) {
    int minn = std::min(a->sn, b->sn);
    int i;
    for (i = 0; i < minn; i++) {
        a->s[i] -= b->s[i];
        if (a->s[i] < 0) {
            a->s[i] += 10;
            a->s[i + 1]--;
        }
    }
    for (; a->s[i] < 0; i++) {
        a->s[i] += 10;
        a->s[i + 1]--;
    }
    a->trimZeroes();
}

void divBy2(BigNum *a) {
    for (int i = 0; i < a->sn; i++) {
        if (a->s[i] % 2 == 0) {
            a->s[i] /= 2;
        } else {
            a->s[i - 1] += 5;
            a->s[i] /= 2;
        }
    }
    a->trimZeroes();
}

void solve() {
    BigNum total, delta;
    scanf("%s%s", total.s, delta.s);
    total.sn = strlen(total.s);
    delta.sn = strlen(delta.s);
    total.revert();
    delta.revert();
    total.toInts();
    delta.toInts();
    BigNum a = total, b = total;
    subtract(&a, &delta);
    divBy2(&a);
    subtract(&b, &a);
    a.revert();
    b.revert();
    a.toChars();
    b.toChars();
    printf("%s\n%s\n", b.s, a.s);
}

int main(int argc, char **argv) {
    // #ifndef ONLINE_JUDGE
    //   freopen("in", "r", stdin);
    // #endif
    for (int i = 0; i < 10; i++) {
        solve();
    }
    return 0;
}
