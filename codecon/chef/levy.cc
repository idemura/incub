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

using namespace std;

using i64 = long long int;
using i32 = int;

constexpr int INF = 0x7fffffff;
constexpr int MOD = 1000000007;

int sqrt_int(int n) {
    return static_cast<int>(sqrt(n));
}

vector<int> primes(int n) {
    vector<int> sieve(n + 1), result;
    auto i = 2, imax = sqrt_int(n);
    for (; i <= imax; i++) {
        if (sieve[i] != 0) continue;
        result.push_back(i);
        for (int j = i * i; j <= n; j += i) {
            sieve[j] = 1;
        }
    }
    for (; i <= n; i++) {
        if (sieve[i] == 0) result.push_back(i);
    }
    return move(result);
}

int main(int argc, char **argv) {
    ios_base::sync_with_stdio(false);
    constexpr int kNum = 10000;
    vector<int> c(kNum + 1);
    auto ps = primes(kNum);
    for (auto a : ps) {
        for (auto b : ps) {
            int n = a + 2 * b;
            if (n >= c.size()) break;
            c[n]++;
        }
    }
    int t;
    cin >> t;
    for (; t > 0; t--) {
        int n;
        cin >> n;
        cout << c[n] << endl;
    }
    return 0;
}
