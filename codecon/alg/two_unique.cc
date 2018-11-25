#include "base.h"

pair<int, int> two_unique(const vector<int> &a) {
    CHECK(a.size() % 2 == 0);
    auto bit_xor = 0;
    for (auto e : a)
        bit_xor ^= e;
    // Find set bit in `bit_xor`. Say, j-th bit. This means two distinct numbers
    // we have are distinct at bit j. We split array on two: one with j-th bit
    // set, another with bit j-th cleared. In each sub array, we have one
    // distinct and apply xor method.
    CHECK(bit_xor != 0);
    auto mask = bit_xor & -bit_xor;
    auto x = 0, y = 0;
    for (auto e : a) {
        if (e & mask)
            x ^= e;
        else
            y ^= e;
    }
    return make_pair(x, y);
}

template <class T>
pair<T, T> ord_pair(const pair<T, T> &p) {
    return p.first < p.second ? make_pair(p.first, p.second)
                              : make_pair(p.second, p.first);
}

void test1() {
    vector<int> a{1, 3, 6, 5, 4, 5, 1, 6};
    auto uniq = ord_pair(two_unique(a));
    CHECK(uniq == make_pair(3, 4));
}

void test2() {
    vector<int> a{1, 4};
    auto uniq = ord_pair(two_unique(a));
    CHECK(uniq == make_pair(1, 4));
}

void test3() {
    vector<int> a{1, 9, 6, 3, 5, 4, 5, 1, 6, 3};
    auto uniq = ord_pair(two_unique(a));
    CHECK(uniq == make_pair(4, 9));
}

int main(int argc, char **argv) {
    ios_base::sync_with_stdio(false);
    test1();
    test2();
    test3();
    cout << "TESTS PASSED." << endl;
    return 0;
}
