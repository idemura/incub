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

struct seg {
    seg *prev;
    int l, r
    int parity;
};

int N, K;
seg ss[5000];
map<int, ss*> lp, rp;

bool left_less(const seg& a, const seg& b)
{
    if (a.l == b.l)
        return a.r < b.r;
    else
        return a.l < b.l;
}

bool check(int s)
{

}

int main()
{
#ifndef ONLINE_JUDGE
    freopen("in", "r", stdin);
#endif
    int i, j;

    scanf("%d%d", &N, &K);
    for (i = 0; i < K; i++) {
        char buf[16];
        scanf("%d%d%s", &ss[i].l, &ss[i].r, buf);
        if (ss[i].l > ss[i].r) {
            swap(ss[i].l, ss[i].r);
        }
        ss[i].i = i;
        ss[i].parity = buf[0] == 'o';
        if (!check(i)) {
            break;
        }
        lp[i]
    }

    printf("%d\n", i < K? i: -1);
    return 0;
}
