#include <algorithm>
#include <stdio.h>
#include <utility>
#include <vector>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))

using namespace std;

typedef long long int lli;

// Naive recursive way.
int knapsackNaiveRec(
        int n, int *weight, int *profit, int i, int total, int weight_max) {
    if (weight_max < 0) {
        return 0;
    }
    if (i == n) {
        return total;
    }
    int p1 = knapsackNaiveRec(
            n,
            weight,
            profit,
            i + 1,
            total + profit[i],
            weight_max - weight[i]);
    int p2 = knapsackNaiveRec(n, weight, profit, i + 1, total, weight_max);
    return max(p1, p2);
}

int knapsack(int n, int *weight, int *profit, int weight_max) {
    int i, j = 1, k, max_profit = 0;
    vector<int> ps(weight_max + 1); // Function weight -> max profit.
    vector<int> ws(weight_max + 1); // Possible weights.
    ws[0] = 0;
    for (i = 0; i < n; i++) {
        int j0 = j;
        for (k = 0; k < j0; k++) {
            int w = ws[k] + weight[i];
            if (w > weight_max) {
                continue;
            }
            int p = ps[ws[k]] + profit[i];
            if (ps[w] == 0) {
                ws[j] = w;
                j++;
                ps[w] = p;
            } else {
                if (p > ps[w]) {
                    ps[w] = p;
                }
            }
            max_profit = max(ps[w], max_profit);
        }
    }
    return max_profit;
}

int main() {
#ifndef ONLINE_JUDGE
    freopen("in", "r", stdin);
#endif
    int i, n, weight_max;
    scanf("%d%d", &weight_max, &n);
    vector<int> weight(n), profit(n);
    for (i = 0; i < n; i++) {
        scanf("%d", &weight[i]);
    }
    for (i = 0; i < n; i++) {
        scanf("%d", &profit[i]);
    }
    int k = knapsack(n, &weight[0], &profit[0], weight_max);
    int k_check = knapsackNaiveRec(n, &weight[0], &profit[0], 0, 0, weight_max);
    printf("%d - %d\n", k, k_check);
    return 0;
}
