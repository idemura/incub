#include "base.h"

struct FibMat {
    int a11, a12, a21, a22;
    FibMat(int a11, int a12, int a21, int a22):
            a11(a11),
            a12(a12),
            a21(a21),
            a22(a22) {}
    FibMat(): FibMat(1, 0, 0, 1) {}
    // this * m
    void mult(FibMat m) {
        i64 b11 = a11, b12 = a12, b21 = a21, b22 = a22;
        a11 = (b11 * m.a11 + b12 * m.a21) % MOD;
        a12 = (b11 * m.a12 + b12 * m.a22) % MOD;
        a21 = (b21 * m.a11 + b22 * m.a21) % MOD;
        a22 = (b21 * m.a12 + b22 * m.a22) % MOD;
    }
};

// k-th Fibonacci number modulo MOD, where:
// fib(0) = 0,
// fib(1) = 1,
// fib(2) = 1,
// fib(3) = 2,
// and so on.
int fib(int k) {
    if (k <= 0) return 0;
    // if (k == 1) return 1;
    FibMat m(0, 1, 1, 1), a;
    for (; k != 0; k >>= 1) {
        if (k & 1) {
            a.mult(m);
        }
        m.mult(m);
    }
    return a.a12;
}

int main(int argc, char **argv) {
    ios_base::sync_with_stdio(false);
    CHECK(fib(0) == 0);
    CHECK(fib(1) == 1);
    CHECK(fib(2) == 1);
    CHECK(fib(3) == 2);
    CHECK(fib(4) == 3);
    CHECK(fib(5) == 5);
    CHECK(fib(6) == 8);
    CHECK(fib(7) == 13);
    CHECK(fib(8) == 21);
    CHECK(fib(9) == 34);
    CHECK(fib(10) == 55);
    cout << "TESTS PASSED." << endl;
    return 0;
}
