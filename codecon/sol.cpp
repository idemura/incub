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
    int l, r, i;
    int odd;
    int visit;
};

int N, K;
seg ss[5000];
int al[5000][5005];
int break_at = INF;

bool left_less(const seg& a, const seg& b)
{
    return a.l < b.l;
}

void dfs(int v, int odd)
{
    int i;
    if (odd != ss[v].odd) {
        break_at = min(v,

    if (ss[v].visit) {
        return;
    }
    ss[v].visit = 1;
    for (i = 0; al[v][i] >= 0; i++) {
        int odd_i = ss[al[v][i]].odd ^ odd;
        if (
    }
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
        ss[i].odd = buf[0] == 'o';
    }

    sort(ss, ss + K, left_less);
    for (i = 0; i < K; i++) {
        int left_i = lower_bound(ss, ss + K, ss[i].r) - ss;
        for (j = left_i; j < K && ss[j].l == ss[i].r; j++) {
            al[ss[i].i][j - left_i] = j;
        }
        al[ss[i].i][j - left_i] = -1;
    }

    // Do DFS
    for (i = 0; i < K; i++) {
        dfs(i, ss[i].odd);
    }

    if (val_range < INF) {
        printf("%d\n", val_range);
    } else {
        printf("-1\n");
    }
    return 0;
}
