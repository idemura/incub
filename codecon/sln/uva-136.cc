// See http://uva.onlinejudge.org/external/1/136.html
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
    static const int N = 1500;
    int ns[N];
    int i2 = 0, i3 = 0, i5 = 0;
    ns[0] = 1;
    int n = 1;
    while (n < N) {
        for (; ns[i2] * 2 <= ns[n - 1]; i2++)
            ;
        for (; ns[i3] * 3 <= ns[n - 1]; i3++)
            ;
        for (; ns[i5] * 5 <= ns[n - 1]; i5++)
            ;
        int x2 = ns[i2] * 2;
        int x3 = ns[i3] * 3;
        int x5 = ns[i5] * 5;
        if (x2 < x3 && x2 < x5) {
            ns[n] = x2;
            i2++;
        } else {
            if (x3 < x5) {
                ns[n] = x3;
                i3++;
            } else {
                ns[n] = x5;
                i5++;
            }
        }
        // printf("%d ", ns[n]);
        n++;
    }
    // printf("\n");
    printf("The 1500'th ugly number is %d.\n", ns[N - 1]);
    return 0;
}
