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

int main() {
    // Has to be more than 10'000 because we scan until find -1.
    constexpr int kMaxBirds{10002};
    int n, w, b, x;
    scanf("%d%d%d%d", &n, &w, &b, &x);
    vector<int> c(n), cost(n);
    for (int i = 0; i < n; i++) {
        scanf("%d", &c[i]);
    }
    for (int i = 0; i < n; i++) {
        scanf("%d", &cost[i]);
    }
    // dp[k] is max money we can have if we bought k birds
    vector<i64> dp(kMaxBirds, -1);
    dp[0] = w;
    for (int i = 0; i < n; i++) {
        vector<i64> dp_dst(kMaxBirds, -1);
        for (int j = 0; dp[j] >= 0; j++) {
            i64 money = min(dp[j] + i64{x}, i64{w} + j * i64{b});
            // @k is how many birds we buy in i-th nest
            for (int k = 0; k <= c[i]; k++) {
                i64 m = money - k * i64{cost[i]};
                if (m < 0) {
                    break;
                }
                if (dp_dst[j + k] < 0 || m > dp_dst[j + k]) {
                    dp_dst[j + k] = m;
                }
            }
        }
        dp = move(dp_dst);
    }
    int max_birds = 0;
    for (; dp[max_birds] >= 0; max_birds++) {
        // empty
    }
    printf("%d\n", max_birds - 1);
    return 0;
}
