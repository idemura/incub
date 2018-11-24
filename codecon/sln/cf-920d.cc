#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <deque>
#include <map>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

using namespace std;

using i32 = int32_t;
using i64 = int64_t;
using pii = std::pair<i32, i32>;
using pll = std::pair<i64, i64>;

int dp[5000];
int iv[5000];
bool used[5000];

int div_up(int a, int b) {
    return (a + b - 1) / b;
}

void log_pour(int n, int s, int d) {
    if (n > 0) {
        printf("%d %d %d\n", n, s + 1, d + 1);
    }
}

int main() {
    int n, k, v;
    scanf("%d%d%d", &n, &k, &v);
    for (int i = 0; i < k; i++) {
        dp[i] = -1;
    }
    int vmax = 0;
    deque<int> q;
    for (int i = 0; i < n; i++) {
        scanf("%d", &iv[i]);
        if (iv[i] == 0) continue;
        auto v_mod = iv[i] % k;
        if (dp[v_mod] < 0 || i < dp[v_mod]) {
            dp[v_mod] = i;
            q.push_back(v_mod);
        }
        vmax += iv[i];
    }
    if (vmax < v) {
        printf("NO\n");
        return 0;
    }
    if (v == 0) {
        printf("YES\n");
        log_pour(div_up(iv[0], k), 0, 1);
        return 0;
    }
    if (v % k == 0) {
        // As special case: it is always possible
        // Example:
        // 2 4 4
        // 2 3
        // Output:
        // YES
        // 1 2 1
        // 1 1 2
        printf("YES\n");
        for (int i = 1; i < n; i++) {
            log_pour(div_up(iv[i], k), i, 0);
        }
        log_pour(v / k, 0, 1);
        return 0;
    }
    while (!q.empty()) {
        auto v_mod = q.front();
        q.pop_front();
        assert(dp[v_mod] >= 0);
        for (int i = dp[v_mod] + 1; i < n; i++) {
            if (iv[i] == 0) continue;
            auto v2 = (v_mod + iv[i]) % k;
            if (dp[v2] < 0 || i < dp[v2]) {
                dp[v2] = i;
                q.push_back(v2);
            }
        }
    }
    if (dp[v % k] < 0) {
        printf("NO\n");
        return 0;
    }
    printf("YES\n");
    int v0 = 0;
    int r = v % k;
    int j0 = dp[r];
    do {
        assert(dp[r] >= 0);
        used[dp[r]] = true;
        v0 += iv[dp[r]];
        if (dp[r] != j0) {
            log_pour(div_up(iv[dp[r]], k), dp[r], j0);
        }
        r = (k + r - iv[dp[r]] % k) % k;
    } while (v0 % k != v % k);
    for (int j = dp[j0]; v0 % k != v % k; j = dp[j]) {
        used[j] = true;
        v0 += iv[j];
        log_pour(div_up(iv[j], k), dp[j], j0);
    }
    assert(v0 % k == v % k);
    int j1 = -1;
    int v1 = 0;
    for (int i = 0; i < n; i++) {
        if (used[i]) continue;
        v1 += iv[i];
        if (j1 < 0) {
            j1 = i;
        } else {
            log_pour(div_up(iv[i], k), i, j1);
        }
    }
    if (v0 > v) {
        // j1 may be -1 if we pour all in one!
        log_pour((v0 - v) / k, j0, (j0 + 1) % n);
    } else if (v0 < v) {
        assert(j1 >= 0);
        log_pour((v - v0) / k, j1, j0);
    }
    return 0;
}
