#include <algorithm>
#include <array>
#include <cmath>
#include <iostream>
#include <map>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

using namespace std;

using i64 = long long int;
using pii = std::pair<int, int>;

namespace {
i64 n = 0, k = 0;
i64 gcd = 0;
i64 ap_rem = 0;
} // namespace

void check(i64 divisor, i64 n1) {
    // Estimation on max k we can have with AP:
    auto k1 = (int)sqrt(2 * n1);
    if (k <= k1) {
        auto ap = k * (k - 1) / 2; // sum(1, 2, ... (k-1))
        if (ap <= n1 && n1 - ap >= k && divisor > gcd) {
            gcd = divisor;
            ap_rem = n1 - ap;
        }
    }
}

int main() {
    cin >> n >> k;
    for (i64 i = 1, j = n; i <= j;) {
        if (n % i == 0) {
            auto n1 = n / i;
            check(i, n1);
            check(n1, i);
        }
        i++;
        j = n / i;
    }
    if (gcd == 0) {
        printf("-1\n");
    } else {
        for (int i = 1; i < k; i++) {
            cout << gcd * i << " ";
        }
        cout << gcd * ap_rem << "\n";
    }
    return 0;
}
