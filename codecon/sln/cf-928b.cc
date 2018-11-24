#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <map>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

using namespace std;

using i32 = int32_t;
using i64 = int64_t;
using pii = std::pair<int, int>;

// @m and @n are boundaries (inclusive)
int around(int i, int m, int n, int k) {
    if (m > n) {
        return 0;
    }
    int l = max(i - k, m);
    int r = min(i + k, n);
    return r - l + 1;
}

int main() {
    int n = 0, k = 0;
    scanf("%d%d\n", &n, &k);
    vector<int> v(n + 1), dp(n + 1);
    for (int i = 1; i <= n; i++) {
        scanf("%d", &v[i]);
    }
    for (int i = 1; i <= n; i++) {
        if (v[i] == 0) {
            dp[i] = around(i, 1, n, k);
        } else {
            int delta = 0;
            if (v[i] + k + 1 <= n) {
                delta = around(i, v[i] + k + 1, n, k);
            }
            dp[i] = dp[v[i]] + delta;
        }
    }
    for (int i = 1; i <= n; i++) {
        printf("%d ", dp[i]);
    }
    printf("\n");
    return 0;
}
