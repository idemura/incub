#include <algorithm>
#include <functional>
#include <iostream>
#include <list>
#include <map>
#include <math.h>
#include <memory>
#include <queue>
#include <set>
#include <sstream>
#include <stdlib.h>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#define ARRAY_SIZEOF(A) (sizeof(A) / sizeof(A[0]))
#define NON_COPYABLE(C)                                                        \
    C(const C &) = delete;                                                     \
    C &operator=(const C &) = delete;

#define CHECK(E)                                                               \
    do {                                                                       \
        if (!(E)) {                                                            \
            cout << "CHECK failed at " << __FILE__ << "@" << __LINE__ << endl; \
            exit(EXIT_FAILURE);                                                \
        }                                                                      \
    } while (false)

using namespace std;

using i64 = long long int;
using i32 = int;

constexpr char kEol[] = "\n";
constexpr int INF = 0x7fffffff;
constexpr int MOD = 1000000007;

// This type is movable to guarantee efficient return.
struct Subsums {
    set<i64> subsums;
    // Total in the array (mod m).
    i64 total = 0;
};

i64 get_lessoreq(const set<i64> &s, i64 n) {
    auto lb = s.lower_bound(n);
    if (lb == s.end()) {
        return *s.rbegin();
    }
    if (*lb > n && lb != s.begin()) {
        --lb;
    }
    return *lb <= n ? *lb : -1;
}

Subsums rec_step(const vector<int> &a, i64 mod, int l, int r, i64 &max_mod) {
    Subsums st;
    if (r - l == 1) {
        auto x_mod = a[l] % mod;
        st.total = x_mod;
        st.subsums.insert(0); // Empty subarray.
        st.subsums.insert(x_mod);
        max_mod = max(max_mod, x_mod);
    } else {
        auto m = l + (r - l) / 2;
        auto st_l = rec_step(a, mod, l, m, max_mod);
        auto st_r = rec_step(a, mod, m, r, max_mod);
        // Left and right already checked. Make ranges of one element on left
        // and maximum possible on the right.
        for (auto x : st_l.subsums) {
            // We store sum from the beginning. So, the sum to the end is total
            // minus sum to the left.
            auto r_sum = (st_l.total + mod - x) % mod;
            // Apparently, max possible sum is m-1. Try to find this in the
            // right part.
            auto max_possible = mod - 1 - r_sum;
            auto le = get_lessoreq(st_r.subsums, max_possible);
            if (le < 0) continue;
            max_mod = max(max_mod, r_sum + le);
        }
        // Merge two parts.
        st.total = (st_l.total + st_r.total) % mod;
        for (auto x : st_r.subsums) {
            st_l.subsums.insert((x + st_l.total) % mod);
        }
        st.subsums = move(st_l.subsums);
    }
    return st;
}

i64 max_subarray_mod(const vector<int> &a, i64 m) {
    i64 max_mod = 0;
    rec_step(a, m, 0, a.size(), max_mod);
    return max_mod;
}

int main() {
    ios_base::sync_with_stdio(false);
    int t = 0;
    cin >> t;
    while (t-- > 0) {
        i64 n, m;
        cin >> n >> m;
        vector<int> a(n);
        for (int i = 0; i < n; i++) {
            cin >> a[i];
        }
        cout << max_subarray_mod(a, m) << endl;
    }
    return 0;
}
