#include <algorithm>
#include <cmath>
#include <iostream>
#include <vector>

using namespace std;

using i64 = long long int;

constexpr int MOD = 1000000007;

i64 div_all(i64 n, int p) {
    do {
        n /= p;
    } while (n % p == 0);
    return n;
}

i64 tot(i64 n, const vector<int> &sieve, const vector<int> &primes) {
    if (n == 1) {
        return 1;
    }
    i64 r = n;
    int j = 0;
    while (n >= sieve.size() && j < primes.size()) {
        if (n % primes[j] == 0) {
            r -= r / primes[j];
            n = div_all(n, primes[j]);
        }
        j++;
    }
    if (j < primes.size()) {
        while (n != 1) {
            r -= r / sieve[n];
            n = div_all(n, sieve[n]);
        }
    }
    // Big prime
    if (n > 1) {
        r -= r / n;
    }
    return r;
}

int eval(i64 n, i64 k, const vector<int> &sieve, const vector<int> &primes) {
    i64 r = tot(n, sieve, primes);
    for (int j = 2; j <= k; j++) {
        if ((j & 1) == 1) {
            r = tot(r, sieve, primes);
        }
        if (r == 1) {
            break;
        }
    }
    return r % MOD;
}

int main() {
    i64 n, k;
    cin >> n >> k;
    int sqrt_n = (int)sqrt(n);
    vector<int> sieve(sqrt_n + 1), primes;
    for (int i = 2; i < sieve.size(); i++) {
        if (sieve[i] == 0) {
            primes.push_back(i);
            sieve[i] = i;
            for (int j = i * i; j < sieve.size(); j += i) {
                if (sieve[j] == 0) {
                    sieve[j] = i;
                }
            }
        }
    }
    cout << eval(n, k, sieve, primes) << endl;
    return 0;
}
