#include <iostream>
#include <algorithm>
#include <vector>
#include <utility>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))

using namespace std;

typedef long long int lli;

int knapsack(int n, int *weight, int *profit, int i, int total, int weight_max)
{
    if (weight_max < 0) {
        return 0;
    }
    if (i == n) {
        return total;
    }
    int p1 = knapsack(n, weight, profit, i + 1,
        total + profit[i],
        weight_max - weight[i]);
    int p2 = knapsack(n, weight, profit, i + 1,
        total,
        weight_max);
    return max(p1, p2);
}

int knapsack_dp(int n, int *weight, int *profit, int weight_max)
{
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

int main()
{
#ifndef ONLINE_JUDGE
    freopen("in", "r", stdin);
#endif
    int i, n, weight_max;
    cin >> weight_max >> n;
    vector<int> weight(n), profit(n);
    for (i = 0; i < n; i++) {
        cin >> weight[i];
    }
    for (i = 0; i < n; i++) {
        cin >> profit[i];
    }
    int k_check = knapsack(n, &weight[0], &profit[0], 0, 0, weight_max);
    int k = knapsack_dp(n, &weight[0], &profit[0], weight_max);
    cout << k << " - " << k_check << endl;
    return 0;
}
