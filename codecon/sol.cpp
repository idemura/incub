#include <algorithm>
#include <vector>
#include <utility>
#include <stdio.h>
#include <math.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define INF 0x7fffffff
#define MOD 1000000007

using namespace std;

typedef long long int lli;

int N, K;

int main()
{
#ifndef ONLINE_JUDGE
    freopen("in", "r", stdin);
#endif
    int i, j;

    scanf("%d%d", &N, &K);
    for (i = 0; i < K; i++) {
        char buf[16];
        int l, r;
        scanf("%d%d%s", &l, &r, buf);
        bool odd = buf[0] == 'o';
    }

    return 0;
}
