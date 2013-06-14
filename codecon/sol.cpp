// #include <iostream>
#include <algorithm>
#include <vector>
#include <utility>
#include <stdio.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))

using namespace std;

typedef long long int lli;

int bits[32], b[32];

int main()
{
#ifndef ONLINE_JUDGE
    freopen("in", "r", stdin);
#endif
    const int num_bits = 8;
    int i, j, k, n, m, x;
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
    printf("bits:\n");
    for (i = 0; i <= num_bits; i++) {
        printf("%d ", bits[i]);
    }
    printf("\n");
    // sort(cs, cs + n);
    for (i = 0; i < m; i++) {
        scanf("%d", &x);
        b[x]++;
    }
    int max_arrays = 0;
    i = 0;
    j = 0;
    for (i = 0; i <= num_bits; i++) {
        printf("alloc %d of size 2^%d==%d\n", b[i], i, 1<<i);
        j = i;
        while (b[i] && j <= num_bits) {
            printf("b[i] = %d, not zero\n", b[i]);
            printf("j = %d\n", j);
            lli mem = (lli)bits[j] << j;
            printf("mem: bits[%d] %d => %lld\n", j, bits[j], mem);
            lli req = (lli)b[i] << i;
            printf("required: %lld\n", req);
            if (mem >= req) {
                printf("have more memory than required\n");
                max_arrays += b[i];
                // printf("  max_arrays %d (delta is b[i] = %d)\n", max_arrays, b[i]);
                b[i] = 0;
                mem -= req;
                printf("  mem without req %lld\n", mem);
                lli r = mem - ((mem >> j) << j);
                printf("  remainder %lld\n", r);
                if (r < 0) {
                    printf("!!!\n");
                }
                bits[j] = mem >> j;
                printf("  bits[%d] now %d\n", j, bits[j]);
                for (k = 0; r; k++, r >>= 1) {
                    if (r & 1)
                        bits[k]++;
                }
                printf("bits:\n");
                for (k = 0; k <= num_bits; k++) {
                    printf("%d ", bits[k]);
                }
                printf("\n");
            } else {
                printf("need more space\n");
                int fit = mem >> i;
                printf("  fit %d\n", fit);
                max_arrays += fit;
                printf("  max_arrays %d\n", max_arrays);
                b[i] -= fit;
                bits[j] = 0;
                printf("  b[%d] %d\n", i, b[i]);
                printf("  zero %d-th bit\n", j);
                j++;
            }
        }
        if (!b[i]) {
            printf("b[%d] is zero, next big step\n", i);
        } else {
            printf("couldn't find space, can break main cycle?\n");
        }
    }
    printf("%d\n", max_arrays);
    return 0;
}
