#include <algorithm>
#include <functional>
#include <iostream>
#include <list>
#include <map>
#include <math.h>
#include <random>
#include <sstream>
#include <stdio.h>
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
constexpr int MOD = 1000000007;

inline int clamp_mod(int n) {
    return n < MOD ? n : n - MOD;
}

void solve(const vector<int> &a, int b, vector<int> &r) {
    vector<int> sum(r.size());
    sum[b] = r[b] = 1;
    for (int i = b - 1; i >= 0; i--) {
        if (a[i] == 0) {
            r[i] = 0;
            sum[i] = sum[i + 1];
        } else {
            // [i + a[i] + 1] is guaranteed to be in range.
            r[i] = clamp_mod(sum[i + 1] - sum[i + a[i] + 1] + MOD);
            sum[i] = clamp_mod(sum[i + 1] + r[i]);
        }
    }
}

int main(int argc, char **argv) {
    ios_base::sync_with_stdio(false);
    int n, b, q;
    cin >> n >> b;
    b--;
    vector<int> a(n);
    for (int i = 0; i < n; i++) {
        cin >> a[i];
    }
    vector<int> r(n);
    solve(a, b, r);
    cin >> q;
    for (int i = 0; i < q; i++) {
        int t;
        cin >> t;
        cout << r[t - 1] << endl;
    }
    return 0;
}
