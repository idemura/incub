#include <bits/stdc++.h>

using namespace std;

int main() {
    int n = 0, m = 0;
    scanf("%d%d", &n, &m);
    if (n == 1) {
        printf("0\n");
        return 0;
    }
    // @ec - edges adjacent to i-th vetrex.
    // @xor_adj_v - bit xor of adjacent vertices, will reveal us the only
    // neighour when ec[i] is 1.
    vector<int> ec(n), xor_adj_v(n);
    for (int i = 0; i < m; i++) {
        int a, b;
        scanf("%d%d", &a, &b);
        a--;
        b--;
        ec[a]++;
        ec[b]++;
        xor_adj_v[a] ^= b;
        xor_adj_v[b] ^= a;
    }
    vector<int> q;
    for (int i = 0; i < n; i++) {
        if (ec[i] == 1) q.push_back(i);
    }
    vector<int> max_tree(n);
    int j = 0;
    while (n - j > 2) {
        ec[q[j]]--;
        auto w = xor_adj_v[q[j]];
        ec[w]--;
        xor_adj_v[w] ^= q[j];
        max_tree[w] = max(max_tree[w], max_tree[q[j]] + 1);
        if (ec[w] == 1) q.push_back(w);
        j++;
    }
    printf("%d\n", max_tree[q[j]] + max_tree[q[j + 1]] + 1);
    return 0;
}
