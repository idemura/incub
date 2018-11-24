#include <algorithm>
#include <chrono>
#include <cmath> // Overloads for abs.
#include <cstdlib>
#include <functional>
#include <iostream>
#include <limits>
#include <list>
#include <map>
#include <memory>
#include <random>
#include <set>
#include <sstream>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

using namespace std;

using i64 = long long int;
using u64 = unsigned i64;
using i32 = int;
using u32 = unsigned i32;

constexpr char kEol[] = "\n";
constexpr int INF = 0x7fffffff;
constexpr int MOD = 1000000007;

constexpr i64 mod(i64 n) {
    return n % MOD;
}

int mpow(i64 a, int k, int p) {
    i64 x = 1;
    for (; k; k >>= 1) {
        if (k & 1) {
            x *= a;
            x %= p;
        }
        a *= a;
        a %= p;
    }
    return (int)x;
}

int minv(int a, int p) {
    return mpow(a, p - 2, p);
}

const i64 k2inv = minv(2, MOD);

i64 naive(i64 n, i64 m) {
    i64 s = 0;
    for (int i = 1; i <= m; i++) {
        s += n % i;
    }
    return mod(s);
}

i64 solve(i64 n, i64 m) {
    if (m == 1) return 0;
    i64 s = 0;
    if (m > n) {
        s = mod(mod(n) * mod(m - n));
        m = n;
    }
    i64 u = n;
    for (i64 q = 1;; q++) {
        i64 l = n / (q + 1) + 1;
        if (m < l) continue;
        if (m < u) u = m;
        i64 num_members = u - l + 1;
        if (num_members <= 1) break;
        i64 r0 = n % u;
        i64 r1 = n % l;
        s = mod(mod(mod(r0 + r1) * mod(num_members)) * k2inv + s);
        u = l - 1;
    }
    for (int i = 1; i <= u; i++) {
        s = mod(s + n % i);
    }
    return s;
}

int main(int argc, char **argv) {
    ios_base::sync_with_stdio(false);
    i64 n, m;
    cin >> n >> m;
    cout << solve(n, m) << endl;
    // cout<<"naive: "<<naive(n, m)<<endl;
    return 0;
}
