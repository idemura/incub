#include "base.h"

int minor_bit(int n) {
    return n & -n;
}

// Indices are 1..n (inclusive).
template <class Num>
class BIT {
public:
    explicit BIT(int n): v_(n + 1), a_(n + 1) {}
    size_t size() const {
        return a_.size() - 1;
    }

    void add(int i, Num x) {
        a_[i] += x;
        while (i < v_.size()) {
            v_[i] += x;
            i += i & -i;
        }
    }

    void set(int i, Num x) {
        add(i, x - a_[i]);
    }

    Num get(int i) const {
        return a_[i];
    }

    // Sum of [a..b], inclusive.
    Num sum(int a, int b) const {
        return sum(b) - sum(a - 1);
    }

    // Sum of [0..i], inclusive.
    Num sum(int i) const {
        auto a = Num();
        while (i != 0) {
            a += v_[i];
            i -= i & -i;
        }
        return a;
    }

private:
    vector<Num> v_, a_;
};

// Sum of @k elements we store at index (k - 1).
// O(n log(n))
template <class T>
vector<T> make_bit(vector<T> a) {
    for (int s = 2; s <= a.size(); s += s) {
        for (int i = s - 1; i < a.size(); i += s) {
            a[i] += a[i - s / 2];
        }
    }
    return move(a); // Not sure. Probably NRVO doens't kick in.
}

// @n - number of elements.
// O(log(n))
template <class T>
T bit_sum(const vector<T> &b, int n) {
    T s = 0;
    while (n != 0) {
        s += b[n - 1];
        n -= minor_bit(n);
    }
    return s;
}

// Sum of (last-first) elements starting @first.
template <class T>
T bit_sum_range(const vector<T> &b, int first, int last) {
    return bit_sum(b, last) - bit_sum(b, first);
}

void test1() {
    BIT<double> bit(11);
    bit.add(1, 1);
    CHECK(bit.sum(1, 1) == 1);
    CHECK(bit.sum(1, 2) == 1);
    CHECK(bit.sum(1, 11) == 1);
    bit.add(3, 2);
    CHECK(bit.sum(1, 1) == 1);
    CHECK(bit.sum(1, 2) == 1);
    CHECK(bit.sum(1, 3) == 3);
    CHECK(bit.sum(1, 4) == 3);
    CHECK(bit.sum(1, 5) == 3);
    CHECK(bit.sum(1, 11) == 3);
    bit.add(1, 1);
    CHECK(bit.sum(1, 1) == 2);
    CHECK(bit.sum(1, 2) == 2);
    CHECK(bit.sum(1, 3) == 4);
    CHECK(bit.sum(1, 4) == 4);
    bit.set(1, 0);
    CHECK(bit.sum(1, 1) == 0);
    CHECK(bit.sum(1, 2) == 0);
    CHECK(bit.sum(1, 3) == 2);
    CHECK(bit.sum(1, 4) == 2);
    CHECK(bit.get(1) == 0);
    CHECK(bit.get(2) == 0);
    CHECK(bit.get(3) == 2);
    bit.set(5, 5);
    bit.set(6, 3);
    bit.add(8, 2);
    bit.add(1, 1);
    bit.add(11, -1.5);
    // 1:1, 3:2, 5:5, 6:3, 8:2, 11:-1
    // Test `sum`:
    CHECK(bit.sum(1) == 1);
    CHECK(bit.sum(2) == 1);
    CHECK(bit.sum(3) == 3);
    CHECK(bit.sum(4) == 3);
    CHECK(bit.sum(5) == 8);
    CHECK(bit.sum(6) == 11);
    CHECK(bit.sum(7) == 11);
    CHECK(bit.sum(8) == 13);
    CHECK(bit.sum(9) == 13);
    CHECK(bit.sum(10) == 13);
    CHECK(bit.sum(11) == 11.5);
    CHECK(bit.sum(3, 5) == 7);
    CHECK(bit.sum(4, 6) == 8);
    CHECK(bit.sum(4, 7) == 8);
    CHECK(bit.sum(8, 10) == 2);
    CHECK(bit.sum(8, 11) == 0.5);
    // Test `get`:
    CHECK(bit.get(11) == -1.5);
    CHECK(bit.get(8) == 2);
    CHECK(bit.get(10) == 0);
    CHECK(bit.get(9) == 0);
}

void test2() {
    const vector<int> a{1, 0, 2, 3, -1, 4, -3, 0, 1};
    auto b = make_bit(a);
    for (int i = 0; i < a.size(); i++) {
        for (int j = i; j < a.size(); j++) {
            auto s1 = 0;
            for (int k = i; k < j; k++) {
                s1 += a[k];
            }
            auto s2 = bit_sum_range(b, i, j);
            if (s1 != s2) {
                cout << "TEST FAIL: range " << i << "," << j << " sum is " << s2
                     << " expected " << s1 << endl;
            }
        }
    }
}

int main(int argc, char **argv) {
    ios_base::sync_with_stdio(false);
    test1();
    test2();
    cout << "TESTS PASSED." << endl;
    return 0;
}
