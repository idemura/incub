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

int factorialTrailingZeroesNum(int n) {
    int nz = 0;
    int p5 = 5;
    int max_p5 = INF / 5;
    for (; p5 <= n; p5 *= 5) {
        nz += n / p5;
        if (p5 >= max_p5) {
            break;
        }
    }
    return nz;
}

int main(int argc, char **argv) {
    // #ifndef ONLINE_JUDGE
    //   freopen("in", "r", stdin);
    // #endif
    int t = 0, n;
    scanf("%d", &t);
    for (; t-- > 0;) {
        scanf("%d", &n);
        printf("%d\n", factorialTrailingZeroesNum(n));
    }
    return 0;
}
