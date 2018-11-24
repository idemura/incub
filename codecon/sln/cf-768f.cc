#include <algorithm>
#include <iostream>
#include <string>
#include <vector>

using namespace std;

using i64 = long long int;

constexpr int MOD = 1000000007;

i64 mpow(i64 a, int k) {
    i64 x = 1;
    for (; k; k >>= 1) {
        if (k & 1) {
            x *= a;
            x %= MOD;
        }
        a *= a;
        a %= MOD;
    }
    return (int)x;
}

i64 minv(i64 a) {
    return mpow(a, MOD - 2);
}

i64 mdiv(i64 a, i64 b) {
    return a * minv(b) % MOD;
}

i64 numC(const vector<int> &fact, int n, int k) {
    if (k == 0) {
        return 1;
    }
    if (k > n) {
        return 0;
    }
    return mdiv(mdiv(fact[n], fact[k]), fact[n - k]);
}

int main() {
    int f, w, h;
    cin >> f >> w >> h;
    if (w == 0) {
        // Problem requires that to be 1, not 0.
        cout << "1\n";
        return 0;
    }

    vector<int> fact(2 * max(f, w) + 1);
    fact[0] = 1;
    fact[1] = 1;
    for (int i = 2; i < fact.size(); i++) {
        fact[i] = i64(fact[i - 1]) * i % MOD;
    }

    i64 total = numC(fact, f + w, f);
    // k is number of stacks for wine.
    i64 accepted = 0;
    for (int k = 1; true; k++) {
        // For each k wine stack stacks (2 in the picture below) we can get
        // possible box locatons like follows:
        //   BWBW
        //   WBWB
        //   WBW
        //   BWBWB
        // which is (combinations of w) * (2*C(f-1, k-1) + C(f-1, k-2) + C(f-1,
        // k)) = C(f+1, k).
        auto r = w - (h + 1) * k;
        if (r < 0) {
            break;
        }
        // Combinations of wine stacks higher that h (so h+1 or higher).
        i64 comb_w_into_k = numC(fact, w - h * k - 1, k - 1);
        i64 comb_f = numC(fact, f + 1, k);
        if (comb_f == 0) {
            break;
        }
        accepted = (accepted + comb_w_into_k * comb_f % MOD) % MOD;
    }
    cout << mdiv(accepted, total) << endl;
    return 0;
}
