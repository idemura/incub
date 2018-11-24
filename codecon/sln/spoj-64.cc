#include <algorithm>
#include <assert.h>
#include <limits.h>
#include <map>
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

typedef long long int lli;

int countInversions(int n, int k) {
    static const int N_MAX = 12;
    static const int ROW_MAX_LENGTH = N_MAX * (N_MAX - 1) / 2;

    // +1 Just in case.
    int tab[N_MAX + 1][ROW_MAX_LENGTH + 1] = {};
    // `tab[l]` is mapping from inversions count to the number of arrays have
    // this amount of inversions. `l` is array length.
    tab[1][0] = 1;
    for (int l = 2; l <= n; l++) {
        for (int j = 0; tab[l - 1][j] != 0; j++) {
            for (int i = 0; i < l; i++) {
                int new_inv_count = j + i;
                tab[l][new_inv_count] += tab[l - 1][j];
            }
        }
    }
    return tab[n][k];
}

int main(int argc, char** argv) {
#ifndef ONLINE_JUDGE
    freopen("in", "r", stdin);
#endif
    int t;
    scanf("%d", &t);
    for (int i = 0; i < t; i++) {
        int n, k;
        scanf("%d%d", &n, &k);
        printf("%d\n", countInversions(n, k));
    }
    return 0;
}
