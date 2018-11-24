#include <bits/stdc++.h>

using namespace std;

using i64 = long long int;
using u64 = unsigned i64;
using i32 = int;
using u32 = unsigned i32;

// Result[i] means minimal divisor of i. Use it in `factorize`.
vector<int> sieve(int n) {
    vector<int> factor(n + 1);
    factor[1] = 1;
    auto i = 2, imax = (int)sqrt(n);
    for (; i <= imax; i++) {
        if (factor[i] != 0) continue;
        factor[i] = i;
        for (int j = i * i; j <= n; j += i) {
            if (factor[j] == 0) {
                factor[j] = i;
            }
        }
    }
    for (; i <= n; i++) {
        if (factor[i] == 0) factor[i] = i;
    }
    return factor;
}

bool can_find_remainder() {
    int n, k;
    scanf("%d%d", &n, &k);
    vector<int> c(n);
    auto c_max = k;
    for (auto &m : c) {
        scanf("%d", &m);
        if (m > c_max) c_max = m;
    }
    if (k == 1) return true;
    const auto sv = sieve(c_max);
    // LCM of @c in form of a map factor=>degree.
    vector<int> lcm(c_max + 1);
    vector<int> factors;
    for (auto n : c) {
        while (n != 1) {
            auto f = sv[n];
            auto d = 0;
            do {
                n /= f;
                d++;
            } while (n % f == 0);
            if (lcm[f] < d) lcm[f] = d;
            factors.push_back(f);
        }
    }
    // First, check LCM is no less than k. After check LCM divisable by k. Later
    // check modifies lcm and k.
    i64 p = 1;
    for (auto f : factors) {
        for (int i = 0; i < lcm[f] && p < k; i++) {
            p *= f;
        }
        if (!(p < k)) break;
    }
    if (p < k) return false;

    // Check if divisable by k.
    for (int t = k; t != 1;) {
        auto f = sv[t];
        if (lcm[f] == 0) return false;
        lcm[f]--;
        t /= f;
    }
    return true;
}

int main() {
    if (can_find_remainder()) {
        cout << "Yes\n";
    } else {
        cout << "No\n";
    }
    return 0;
}
