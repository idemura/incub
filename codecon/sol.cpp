// #include <iostream>
#include <algorithm>
#include <vector>
#include <utility>
#include <stdio.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))

using namespace std;

typedef long long int lli;

int bits[32], bl[32];

int main()
{
#ifndef ONLINE_JUDGE
    freopen("in", "r", stdin);
#endif
    int i, j, n, m, x;
    scanf("%d%d", &n, &m);
    // Since all block sizes are 2^n that means every pair of sizes are
    // divisable so greedy is ok.
    for (i = 0; i < n; i++) {
        scanf("%d", &x);
        for (j = 0; x; j++, x >>= 1) {
            if (x & 1)
                bits[j]++;
        }
    }
    // sort(cs, cs + n);
    for (i = 0; i < m; i++) {
        scanf("%d", &x);
        bl[x]++;
    }
    int max_arrays = 0;
    i = 0;
    j = 0;
    for (i = 0; i <= 30; i++) {
        if (bl[i] == 0) {
            continue;
        }
        j = i;
        while (bl[i]) {
            lli per_cl = 1 << (j - i);
            lli can_alloc = bits[j] * per_cl;
            if (bl[i] < can_alloc) {
                max_arrays += bl[i];
                int e = bl[i] / per_cl;
                bits[j] -= e;
                (1 << j) - e * per_cl
                bl[i] = 0;
            } else {
                max_arrays += can_alloc;
                bl[i] -= can_alloc;
                j++;
            }
        }
            max_arrays += bl[i];
        } else {
            bl[i] -= bits[i];
            max_arrays += bits[i];
            for (; j <= 30 && bits[j]; j++);

        }

        // int sz_j = 1 << j;
        printf("cluster %d, block size %d\n", cs[i], sz_j);
        if (sz_j > cs[i]) {
            printf("go next cluster\n");
            i++;
            continue;
        }
        int fit = min(cs[i] / sz_j, bl[j]);
        printf("fit %d\n", fit);
        bl[j] -= fit;
        cs[i] -= fit * sz_j;
        printf("new cs[i] %d, new bl[j] %d\n", cs[i], bl[j]);
        max_arrays += fit;
        printf("max_arrays %d\n", max_arrays);
    }
    printf("%d\n", max_arrays);
    return 0;
}
