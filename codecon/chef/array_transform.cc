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

void solve() {
    int n, k, x;
    scanf("%d%d", &n, &k);
    int mods[11] = {};
    for (int i = 0; i < n; i++) {
        scanf("%d", &x);
        mods[x % (k + 1)]++;
    }
    bool possible = false;
    for (int i = 0; i <= k; i++) {
        if (mods[i] >= n - 1) {
            possible = true;
            break;
        }
    }
    printf("%s\n", possible ? "YES" : "NO");
}

// Transformation in the problem can be replaced with: we can add k+1, can we
// achieve n-1 equal numbers? Since we can add only (k+1) it is possible if we
// have at least n-1 equal mods by k+1.
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
