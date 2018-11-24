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

int n1, l1[50000], n2, l2[50000];
const int SUM = 10000;

void readlist(int *n, int *l) {
    scanf("%d", n);
    for (int i = 0; i < *n; i++) {
        scanf("%d", l + i);
    }
}

int main() {
#ifndef ONLINE_JUDGE
    freopen("in", "r", stdin);
#endif
    int i1 = 0, i2 = 0;
    // First list ascending, second - descending.
    readlist(&n1, l1);
    readlist(&n2, l2);

    const char *ans = "NO";
    for (; i1 < n1 && i2 < n2;) {
        int s = l1[i1] + l2[i2];
        if (s == SUM) {
            ans = "YES";
            break;
        }
        s < SUM ? i1++ : i2++;
    }

    printf("%s\n", ans);
    return 0;
}
