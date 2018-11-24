#include "base.h"

// Good article with formula proof can be found at:
// http://neerc.ifmo.ru/wiki/index.php?title=%D0%9A%D0%BE%D0%B4%D1%8B_%D0%93%D1%80%D0%B5%D1%8F

string binary(int n, int width) {
    string s;
    while (n) {
        s.push_back(n & 1 ? '1' : '0');
        n >>= 1;
    }
    while (s.size() < width) {
        s.push_back('0');
    }
    reverse(s.begin(), s.end());
    return s;
}

// It's important that type is unsigned, because it relies on the fact that
// after shift right, 0 appears in MSB.
// Details at http://e-maxx.ru/algo/gray_code.
u32 encode_gray(u32 n) {
    return n ^ (n >> 1);
}

u32 decode_gray(u32 g) {
    u32 n = 0;
    for (; g; g >>= 1) {
        n ^= g;
    }
    return n;
}

bool one_bit_set(u32 n) {
    return (n & (n - 1)) == 0;
}

int main() {
    constexpr auto n = 3;
    for (u32 i = 0; i < (1u << n); i++) {
        auto g = encode_gray(i);
        cout << binary(g, n) << " -> " << binary(decode_gray(g), n) << endl;
        // Check 1 bit difference.
        if (i > 0) {
            auto diff = encode_gray(i - 1) ^ g;
            CHECK(one_bit_set(diff));
        }
    }
    cout << "TESTS PASSED." << endl;
    return 0;
}
