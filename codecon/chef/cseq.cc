#include <algorithm>
#include <functional>
#include <iostream>
#include <list>
#include <map>
#include <math.h>
#include <random>
#include <sstream>
#include <stdlib.h>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#define ARRAY_SIZEOF(A) (sizeof(A) / sizeof(A[0]))

using namespace std;

using i64 = long long int;
using i32 = int;

constexpr char kEol[] = "\n";
constexpr int INF = 0x7fffffff;
constexpr int MOD = 1000003;

vector<int> fac_mod_tab;

int pow_mod(i64 n, int p) {
    i64 a = n, x = 1;
    for (; p; p >>= 1) {
        if (p & 1) {
            x = x * a % MOD;
        }
        a = a * a % MOD;
    }
    return x % MOD;
}

int inv_mod(int a) {
    return pow_mod(a, MOD - 2);
}

int factorial_mod(int n) {
    i64 pt = pow_mod(fac_mod_tab.back(), n / MOD);
    i64 fr = fac_mod_tab[n % MOD];
    return (pt * fr) % MOD;
}

int bincoef_mod(int n, int k) {
    int m = n - k;
    // Compute C(cn, ck) mod MOD. Trick is for each 1..MOD we know that this
    // residue will happen cn / MOD times => r ^ (cn / MOD).
    if (n / MOD != k / MOD + m / MOD) {
        return 0;
    }
    i64 n_fac = factorial_mod(n);
    i64 k_fac = factorial_mod(k);
    i64 m_fac = factorial_mod(m);
    i64 b = n_fac * inv_mod(k_fac * m_fac % MOD);
    // We eliminated all multiples of MOD but each has quots left.
    if (n / MOD > 0) {
        b %= MOD;
        // With MOD=1000003 we can do up to 3 multiplications in i64 range.
        b *= bincoef_mod(n / MOD, k / MOD);
    }
    return b % MOD;
}

int cseq(int n, int m) {
    // 0 elem: 1 1 1 1
    // 1 elem: 1 2 3 4
    // 2 elem: 1 3 6 10
    // 3 elem: 1 4 10
    // 4 elem: 1 5 15
    // 5 elem: 1 6 21
    // cout<<"n="<<n<<" m="<<m<<endl;
    auto r = bincoef_mod(n + m + 1, m + 1) - 1;
    if (r < 0) r += MOD;
    return r;
}

int main(int argc, char **argv) {
    ios_base::sync_with_stdio(false);
    fac_mod_tab.resize(MOD);
    fac_mod_tab[0] = 1;
    for (int i = 1; i < MOD; i++) {
        fac_mod_tab[i] = i64(i) * i64(fac_mod_tab[i - 1]) % MOD;
    }
    int t = 0;
    cin >> t;
    while (t--) {
        int n, l, r;
        cin >> n >> l >> r;
        cout << cseq(n, r - l) << endl;
    }
    return 0;
}
