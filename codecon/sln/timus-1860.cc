#include <algorithm>
#include <math.h>
#include <stdio.h>
#include <utility>
#include <vector>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define INF 0x7fffffff
#define MOD 1000000007

using namespace std;

typedef long long int lli;

int es[1000000 + 2];
lli ps[1000000 + 2]; // powers

int main() {
#ifndef ONLINE_JUDGE
    freopen("in", "r", stdin);
#endif
    int i, j;

    int n;
    scanf("%d", &n);

    if (n <= 1) {
        printf("1\n");
        return 0;
    }

    int imax = (int)sqrt(n);
    for (i = 2; i <= imax; i++) {
        if (es[i]) continue;
        for (j = i * i; j <= n; j += i) {
            es[j] = i;
        }
    }

    ps[n] = ps[n - 1] = 1;
    for (i = n - 2; i > 0; i--) {
        ps[i] = (ps[i + 1] + ps[i + 2]) % MOD;
    }

    lli c = 1;
    for (i = n; i > 1; i--) {
        if (es[i] == 0) {
            c = (ps[i] + 1) * c % MOD;
        } else {
            ps[es[i]] = (ps[es[i]] + ps[i]) % MOD;
            int q = i / es[i];
            ps[q] = (ps[q] + ps[i]) % MOD;
        }
    }

    printf("%d\n", (int)c);
    return 0;
}
