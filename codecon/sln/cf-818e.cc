#include <algorithm>
#include <array>
#include <cmath>
#include <cstdio>
#include <iostream>
#include <map>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

using namespace std;

using i64 = long long int;
using pii = std::pair<int, int>;

using powers = array<int, 10>;

namespace {
i64 n, k;
vector<int> fact;
powers kpow{};
vector<powers> a;

void factorize_init(int k) {
    int i = 0;
    int max = sqrt(k);
    for (int d = 2; d <= max; d++) {
        if (k % d == 0) {
            fact.push_back(d);
            do {
                kpow[i]++;
                k /= d;
            } while (k % d == 0);
            i++;
        }
    }
    if (k != 1) {
        fact.push_back(k);
        kpow[i]++;
    }
}

void factorize(int k, powers &p) {
    int i = 0;
    for (auto d : fact) {
        while (k % d == 0) {
            p[i]++;
            k /= d;
        }
        i++;
    }
}

// Range inclusive on the right side
pii get_div_range(int start) {
    auto maxj = 0, minj = 0x7fffffff;
    for (int i = 0; i < fact.size(); i++) {
        int j = lower_bound(
                        a.begin() + start,
                        a.end(),
                        a[start - 1][i] + kpow[i],
                        [i](powers const &a, int b) { return a[i] < b; }) -
                a.begin();
        if (j == a.size()) {
            return {-1, -1};
        }
        maxj = max(maxj, j);
    }
    for (int i = 0; i < fact.size(); i++) {
        int j = upper_bound(
                        a.begin(),
                        a.begin() + maxj + 1,
                        a[maxj][i] - kpow[i],
                        [i](int b, powers const &a) { return b < a[i]; }) -
                a.begin();
        minj = min(minj, j);
    }
    return {minj, maxj};
}
} // namespace

int main() {
    cin >> n >> k;
    if (k == 1) {
        cout << (n * (n + 1) / 2) << endl;
        return 0;
    }
    factorize_init(k);
    a.resize(n + 1);
    for (int i = 1; i <= n; i++) {
        int e;
        cin >> e;
        a[i] = a[i - 1];
        factorize(e, a[i]);
    }
    i64 count = 0;
    for (int i = 1; i <= n; i++) {
        auto r = get_div_range(i);
        if (r.first < 0) {
            break;
        }
        count += i64(r.first - i + 1) * i64(n - r.second + 1);
        i = r.first; // and +1 in for
    }
    cout << count << endl;
    return 0;
}
