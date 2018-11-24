#include <algorithm>
#include <assert.h>
#include <map>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <utility>
#include <vector>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define INF 0x7fffffff

typedef long long int lli;

int getCyclicLength(int n) {
    int cl = 1;
    while (n != 1) {
        if (n & 1) {
            n = (3 * n + 1) >> 1;
            cl += 2;
        } else {
            n >>= 1;
            cl += 1;
        }
    }
    return cl;
}

// From problem statement I conclude labyrinth is a tree.
int main(int argc, char **argv) {
    // #ifndef ONLINE_JUDGE
    //   freopen("in", "r", stdin);
    // #endif
    int a, b;
    while (scanf("%d%d", &a, &b) == 2) {
        int i0 = std::min(a, b);
        int i1 = std::max(a, b);
        int cl_max = 1;
        for (int i = i0; i <= i1; i++) {
            cl_max = std::max(cl_max, getCyclicLength(i));
        }
        printf("%d %d %d\n", a, b, cl_max);
    }
    return 0;
}
