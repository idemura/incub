#include <algorithm>
#include <map>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <vector>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define INF 0x7fffffff

using namespace std;

int N;
int M[108][108];

int maxSum(int *as, int as_n, int *i0, int *i1) {
    int i, sum = 0, max_sum = -INF, sum_begin = 0;
    for (i = 0; i < as_n; i++) {
        sum += as[i];
        if (sum > max_sum) {
            max_sum = sum;
            *i0 = sum_begin;
            *i1 = i + 1;
        }
        if (sum < 0) {
            sum = 0;
            sum_begin = i + 1;
        }
    }
    return max_sum;
}

int main(int argc, char **argv) {
#ifndef ONLINE_JUDGE
    freopen("in", "r", stdin);
#endif
    int i, j, k, i0, i1;
    scanf("%d", &N);
    for (i = 0; i < N; i++) {
        for (j = 0; j < N; j++) {
            scanf("%d", &M[i][j]);
        }
    }
    int sum, max_sum = -INF;
    for (i = 0; i < N; i++) {
        for (j = i + 1; j < N; j++) {
            sum = maxSum(M[i], N, &i0, &i1);
            if (sum > max_sum) {
                max_sum = sum;
            }
            for (k = 0; k < N; k++) {
                M[i][k] += M[j][k];
            }
        }
        sum = maxSum(M[i], N, &i0, &i1);
        if (sum > max_sum) {
            max_sum = sum;
        }
    }
    printf("%d\n", max_sum);
    return 0;
}
