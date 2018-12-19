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

struct ExtGCDResult {
    int gcd{}, a{}, b{};
    ExtGCDResult(int gcd, int a, int b): gcd{gcd}, a{a}, b{b} {}
};

ExtGCDResult extGCD(int a, int b) {
    if (b == 0) {
        return {a, 1, 0};
    }
    auto r = extGCD(b, a % b);
    // Represent as matrix multiplication and do in a cycle.
    return {r.gcd, r.b, r.a - (a / b) * r.b};
}

int getDivisor(int n) {
    if (n % 2 == 0) {
        return 2;
    }
    int d = 3;
    int dMax = (int)sqrt(n);
    while (d <= dMax && n % d != 0) {
        d += 2;
    }
    if (d > dMax) {
        return 1;
    }
    return d;
}

int main(int argc, char **argv) {
    int n = 0;
    scanf("%d", &n);
    int d = getDivisor(n);
    if (d == 1) {
        // Prime number
        printf("NO\n");
        return 0;
    }
    int n0 = n;
    while (n % d == 0) {
        n /= d;
    }
    if (n == 1) {
        printf("NO\n");
        return 0;
    }
    int x = n;
    int y = n0 / n;
    auto g = extGCD(x, y);
    assert(g.a * x + g.b * y == g.gcd);
    // One coefficient is positive, another is negative
    printf("YES\n2\n");
    if (g.a < 0) {
        printf("%d %d\n", -g.a, y);
        printf("%d %d\n", x - g.b, x);
    } else {
        printf("%d %d\n", y - g.a, y);
        printf("%d %d\n", -g.b, x);
    }
    return 0;
}
