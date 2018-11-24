// See http://uva.onlinejudge.org/external/1/100.html
#include <algorithm>
#include <stdio.h>
#include <utility>
#include <vector>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))

using namespace std;

typedef long long int lli;

int main() {
#ifndef ONLINE_JUDGE
    freopen("in", "r", stdin);
#endif
    int l, r, i, j;
    while (scanf("%d%d", &l, &r) == 2) {
        int max_cycle = 0;
        for (i = min(l, r); i <= max(l, r); i++) {
            int c = 1;
            for (j = i; j != 1;) {
                if (j & 1)
                    j = j + (j << 1) + 1;
                else
                    j >>= 1;
                c++;
            }
            if (c > max_cycle) max_cycle = c;
        }
        printf("%d %d %d\n", l, r, max_cycle);
    }
    return 0;
}
