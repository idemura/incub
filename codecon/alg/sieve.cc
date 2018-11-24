#include "base.h"

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

// Pairs (factor, degree).
vector<pair<int, int>> factorize(const vector<int> &factor, int n) {
    vector<pair<int, int>> result;
    if (n <= 1) return result;
    auto p = make_pair(factor[n], 1);
    n /= factor[n];
    while (n != 1) {
        if (p.first == factor[n]) {
            p.second++;
        } else {
            result.push_back(p);
            p = make_pair(factor[n], 1);
        }
        n /= factor[n];
    }
    result.push_back(p);
    return result;
}

vector<int> primes(int n) {
    vector<int> sieve(n + 1), result;
    auto i = 2, imax = (int)sqrt(n);
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
    return result;
}

void test0() {
    for (volatile int i = 0; i < 46340; i++) {
        CHECK(i == (int)sqrt(i * i));
    }
}

void test1() {
    auto s = sieve(27);
    CHECK(s == vector<int>({0, 1, 2, 3,  2, 5,  2, 7, 2, 3,  2, 11, 2, 13,
                            2, 3, 2, 17, 2, 19, 2, 3, 2, 23, 2, 5,  2, 3}));
    auto f2 = factorize(s, 2);
    CHECK(f2.size() == 1);
    CHECK(f2[0] == make_pair(2, 1));
    auto f3 = factorize(s, 3);
    CHECK(f3.size() == 1);
    CHECK(f3[0] == make_pair(3, 1));
    auto f4 = factorize(s, 4);
    CHECK(f4.size() == 1);
    CHECK(f4[0] == make_pair(2, 2));
    auto f5 = factorize(s, 5);
    CHECK(f5.size() == 1);
    CHECK(f5[0] == make_pair(5, 1));
    auto f6 = factorize(s, 6);
    CHECK(f6.size() == 2);
    CHECK(f6[0] == make_pair(2, 1));
    CHECK(f6[1] == make_pair(3, 1));
    auto f7 = factorize(s, 7);
    CHECK(f7.size() == 1);
    CHECK(f7[0] == make_pair(7, 1));
    auto f8 = factorize(s, 8);
    CHECK(f8.size() == 1);
    CHECK(f8[0] == make_pair(2, 3));
    auto f9 = factorize(s, 9);
    CHECK(f9.size() == 1);
    CHECK(f9[0] == make_pair(3, 2));
    auto f10 = factorize(s, 10);
    CHECK(f10.size() == 2);
    CHECK(f10[0] == make_pair(2, 1));
    CHECK(f10[1] == make_pair(5, 1));
    auto f11 = factorize(s, 11);
    CHECK(f11.size() == 1);
    CHECK(f11[0] == make_pair(11, 1));
    auto f12 = factorize(s, 12);
    CHECK(f12.size() == 2);
    CHECK(f12[0] == make_pair(2, 2));
    CHECK(f12[1] == make_pair(3, 1));
    auto f18 = factorize(s, 18);
    CHECK(f18.size() == 2);
    CHECK(f18[0] == make_pair(2, 1));
    CHECK(f18[1] == make_pair(3, 2));
}

void test2() {
    CHECK(primes(29) == vector<int>({2, 3, 5, 7, 11, 13, 17, 19, 23, 29}));
}

int main(int argc, char **argv) {
    ios_base::sync_with_stdio(false);
    test0();
    test1();
    test2();
    cout << "TESTS PASSED." << endl;
    return 0;
}
