#include "base.h"

int gcd(int a, int b) {
    while (b != 0) {
        int t = a % b;
        a = b;
        b = t;
    }
    return a;
}

struct ExtGCD {
    int gcd, a, b;
    ExtGCD(int gcd, int a, int b): gcd(gcd), a(a), b(b) {}
};

ExtGCD ext_gcd(int a, int b) {
    if (b == 0) {
        return {a, 1, 0};
    }
    auto r = ext_gcd(b, a % b);
    // Represent as matrix multiplication and do in a cycle.
    return {r.gcd, r.b, r.a - (a / b) * r.b};
}

bool operator==(const ExtGCD &x, const ExtGCD &y) {
    return x.gcd == y.gcd && x.a == y.a && x.b == y.b;
}
bool operator!=(const ExtGCD &x, const ExtGCD &y) {
    return !(x == y);
}

void test1() {
    CHECK(ext_gcd(4, 6) == ExtGCD(2, -1, 1));
    CHECK(ext_gcd(6, 4) == ExtGCD(2, 1, -1));
    CHECK(ext_gcd(9, 4) == ExtGCD(1, 1, -2));
    CHECK(ext_gcd(3, 7) == ExtGCD(1, -2, 1));
    CHECK(ext_gcd(12, 15) == ExtGCD(3, -1, 1));
    CHECK(ext_gcd(10, 15) == ExtGCD(5, -1, 1));
}

int main(int argc, char **argv) {
    ios_base::sync_with_stdio(false);
    test1();
    cout << "TESTS PASSED." << endl;
    return 0;
}
