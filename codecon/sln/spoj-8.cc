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

int vs[120], last[120];
int vs_n, c_n;

// Returns true if sequence `vs[vs_i]` consists of all equal numbers.
bool isConst(int n) {
    for (int i = 1; i < n; i++) {
        if (vs[i - 1] != vs[i]) return false;
    }
    return true;
}

// Returns step after that all differences equal. Sets up `last` array.
int expandToConstDifferences() {
    for (int i = 0, n = vs_n;; i++, n--) {
        last[i] = vs[n - 1];
        if (isConst(n)) {
            return i;
        }
        for (int j = 1; j < n; j++) {
            vs[j - 1] = vs[j] - vs[j - 1];
        }
    }
    return -1; // Never here.
}

void solve() {
    scanf("%d%d", &vs_n, &c_n);
    for (int i = 0; i < vs_n; i++) {
        scanf("%d", vs + i);
    }
    // Expand to differences. With polynomial, it will be all constant or one
    // single value at some step `ci < vs_n`.
    int ci = expandToConstDifferences();
    // Make steps back from differences to values.
    for (int j = 0; j < c_n; j++) {
        vs[j] = last[ci];
    }
    for (; ci-- > 0;) {
        vs[0] += last[ci];
        for (int j = 1; j < c_n; j++) {
            vs[j] += vs[j - 1];
        }
    }
    for (int j = 0; j < c_n; j++) {
        printf("%d ", vs[j]);
    }
    printf("\n");
}

int main(int argc, char **argv) {
    // #ifndef ONLINE_JUDGE
    //   freopen("in", "r", stdin);
    // #endif
    int t = 0;
    scanf("%d", &t);
    for (; t-- > 0;) {
        solve();
    }
    return 0;
}
