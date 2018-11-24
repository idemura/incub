#include <algorithm>
#include <assert.h>
#include <ctype.h>
#include <functional>
#include <limits.h>
#include <map>
#include <math.h>
#include <queue>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <vector>

#define ARRAY_SIZEOF(A) (sizeof(A) / sizeof(A[0]))
#define INF 0x7fffffff
#define NON_COPYABLE(C)                                                        \
    C(const C&);                                                               \
    C& operator=(const C&);

using namespace std;

typedef long long int lli;

const int kMax = 300;
const int kCoins[] = {5 / 5,
                      10 / 5,
                      20 / 5,
                      50 / 5, // Cents
                      100 / 5,
                      200 / 5,
                      500 / 5,
                      1000 / 5,
                      2000 / 5,
                      5000 / 5,
                      10000 / 5}; // 100$

// May save some memory if store `kMax / kCoins[i]` at row i.
long mem[ARRAY_SIZEOF(kCoins)][kMax * 100 / 5 + 1];

long countRec(int n, int ci) {
    if (ci == 0 || n == 0) {
        return 1;
    }
    if (mem[ci][n] == 0) {
        int max_coins = n / kCoins[ci];
        long s = 0;
        for (int i = 0; i <= max_coins; i++) {
            s += countRec(n - i * kCoins[ci], ci - 1);
        }
        mem[ci][n] = s;
    }
    return mem[ci][n];
}

long count(int n) {
    return countRec(n, ARRAY_SIZEOF(kCoins) - 1);
}

int main(int argc, char** argv) {
#ifndef ONLINE_JUDGE
    freopen("in", "r", stdin);
#endif
    double x = 0;
    while (scanf("%lf", &x) == 1) {
        int n = (x + 0.005) * 100 / 5; // Adding 0.005 is crucial to get AC.
        if (n == 0) break;
        printf("%6.2lf%17ld\n", x, count(n));
    }
    return 0;
}
