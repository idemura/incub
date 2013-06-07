#include <iostream>
#include <algorithm>
#include <vector>
#include <utility>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))

using namespace std;

typedef long long int lli;

int table[10][512];

int knapsack(int n, int *weight, int *profit, int i, int total, int weight_max)
{
    if (table[i][weight_max] != 0) {
        return table[i][weight_max];
    }
    int val = 0;
    if (weight_max < 0) {
        val = 0;
    } else if (i == n) {
        val = total;
    } else {
        int p1 = knapsack(n, weight, profit, i + 1,
            total + profit[i],
            weight_max - weight[i]);
        int p2 = knapsack(n, weight, profit, i + 1,
            total,
            weight_max);
        val = max(p1, p2);
    }
    table[i][weight_max] = val;
    return val;
}

int knapsack_dp(int n, int *weight, int *profit, int weight_max)
{
    int i, j = 1, k;

    printf("input weight:\n");
    for (i=0; i<n; i++) {
        printf("%2d ", weight[i]);
    }
    printf("\n");
    printf("input profit:\n");
    for (i=0; i<n; i++) {
        printf("%2d ", profit[i]);
    }

    vector<int> ps(weight_max + 1); // Function weight -> max profit.
    vector<int> ws(weight_max + 1); // Possible weights.
    ws[0] = 0;
    for (i = 0; i < n; i++) {
        int j0 = j;
        printf("update %d weights with w=%d p=%d\n", j0, weight[i], profit[i]);
        for (k = 0; k < j0; k++) {
            int w = ws[k] + weight[i];
            if (w > weight_max) {
                continue;
            }
            if (ps[w] == 0) {
                ws[j++] = w;
            }
            ps[w] = max(ps[w], ps[k] + profit[i]);
        }
    }
    printf("profits:\n");
    for (i=0; i<ps.size(); i++) {
        printf("%2d ", ps[i]);
    }
    printf("\n");
    printf("weights:\n");
    for (i=0; i<ws.size(); i++) {
        printf("%2d ", ws[i]);
    }
    printf("\n");
    for (i = ps.size(); i--; ) {
        if (ps[i])
            break;
    }
    return i < 0? 0: ps[i];
}

int main()
{
#ifndef ONLINE_JUDGE
    freopen("in", "r", stdin);
#endif
    int i, n, weight_max, check;
    cin >> weight_max >> n;
    vector<int> weight(n), profit(n);
    for (i = 0; i < n; i++) {
        cin >> weight[i];
    }
    for (i = 0; i < n; i++) {
        cin >> profit[i];
    }
    cin >> check;
    // int k = knapsack(n, &weight[0], &profit[0], 0, 0, weight_max);
    int k = knapsack_dp(n, &weight[0], &profit[0], weight_max);
    printf("%d - %d\n", k, check);
    return 0;
}
