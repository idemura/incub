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

constexpr i64 kMod{1'000'000'000 + 7};

// Number different sequences ordered sequences a[i] such that sum(a[i]) = n
i64 num_sums(i64 n) {
    // It is 2 ^ (n - 1). Let's write n ones: 111...1. Let's insert `+` in
    // different positions: C[n - 1, k] for k `+`. Total sum(C[n - 1, k]) where
    // k = 0...n - 1 gives us 2 ^ (n - 1).
    i64 p = 1;
    i64 m = 2;
    n--;
    while (n) {
        if (n % 2) {
            p = (p * m) % kMod;
        }
        m = (m * m) % kMod;
        n >>= 1;
    }
    return p;
}

i64 sub(i64 a, i64 b) {
    return (kMod + a - b) % kMod;
}

unordered_map<i64, i64> dp;

i64 solve(i64 n) {
    auto dp_pos = dp.find(n);
    if (dp_pos != dp.end()) {
        return dp_pos->second;
    }
    i64 sum_subparts = 1;
    int d1 = 2;
    int d2 = n / d1;
    while (d1 <= d2) {
        if (n % d1 == 0) {
            sum_subparts += solve(d1);
            if (d1 != d2) {
                sum_subparts += solve(d2);
            }
            sum_subparts %= kMod;
        }
        d1++;
        d2 = n / d1;
    }
    auto v = sub(num_sums(n), sum_subparts);
    dp.emplace(n, v);
    return v;
}

int main() {
    int x = 0, y = 0;
    scanf("%d%d", &x, &y);
    if (y % x != 0) {
        printf("0\n");
        return 0;
    }
    if (x == y) {
        printf("1\n");
        return 0;
    }
    printf("%d\n", (int)solve(y / x));
    return 0;
}
