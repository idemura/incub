#include <algorithm>
#include <assert.h>
#include <map>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <utility>
#include <vector>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define INF 0x7fffffff

typedef long long int lli;

// Fill a into b.
int pour(const int a, const int b, const int c) {
    int va = 0, vb = 0;
    int step = 0;
    for (; va != c && vb != c;) {
        if (va == 0) {
            va = a;
            step++;
        } else if (vb == b) {
            vb = 0;
            step++;
        } else {
            int vol_to_b = std::min(b - vb, va);
            vb += vol_to_b;
            va -= vol_to_b;
            step++;
        }
    }
    return step;
}

int gcd(int a, int b) {
    while (b != 0) {
        int t = a % b;
        a = b;
        b = t;
    }
    return a;
}

int solve(int a, int b, int c) {
    if (c > b && c > a) {
        return -1;
    }
    if (c == 0) {
        return 0;
    }
    if (c == a || c == b) {
        return 1;
    }
    if (c % gcd(a, b)) {
        return -1;
    }
    return std::min(pour(a, b, c), pour(b, a, c));
}

int main(int argc, char **argv) {
    // #ifndef ONLINE_JUDGE
    //   freopen("in", "r", stdin);
    // #endif
    int t = 0, a, b, c;
    scanf("%d", &t);
    for (; t-- > 0;) {
        scanf("%d%d%d", &a, &b, &c);
        printf("%d\n", solve(a, b, c));
    }
    return 0;
}
