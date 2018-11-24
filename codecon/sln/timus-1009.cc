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

int N, K;
int t[20];

int count(int n) {
    if (n == 1) {
        return K;
    }
    if (n == 0) {
        return 1;
    }
    return (K - 1) * count(n - 1) + (K - 1) * count(n - 2);
}

int main() {
#ifndef ONLINE_JUDGE
    freopen("in", "r", stdin);
#endif
    int i;

    scanf("%d%d", &N, &K);

    t[0] = 1;
    t[1] = K;
    for (i = 2; i <= N; i++) {
        t[i] = (K - 1) * (t[i - 1] + t[i - 2]);
    }

    // int c = (K-1) * count(N-1);
    int c = (K - 1) * t[N - 1];
    printf("%d\n", c);

    return 0;
}
