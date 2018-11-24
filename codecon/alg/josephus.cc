#include "base.h"

// Elements are indexed 0 .. n-1, k>=1. At first step, we execute k % n, which
// is at index (k % n - 1, remember k>=1!). Now we solve j(n - 1, k). The only
// difference, j(n - 1, k) counts index from 0, which corresponds to k % n after
// first step, we add modulo n it.
// Base case n = 1 result is 0.
int josephus_rec(int n, int k) {
    if (n == 1) return 0;
    return (josephus_rec(n - 1, k) + k) % n;
}

// This is iterative rewrite of josephus_rec.
int josephus(int n, int k) {
    int r = 0;
    for (int m = 2; m <= n; m++) {
        r = (r + k) % m;
    }
    return r;
}

// For test purpose.
int josephus_simple(int n, int k) {
    vector<int> a(n);
    for (int i = 0; i < n; i++) {
        a[i] = i;
    }
    int j = 0;
    for (int m = n; m != 1; m--) {
        for (int c = 0; c < k; j = (j + 1) % n) {
            if (a[j] >= 0) {
                c++;
                if (c == k) a[j] = -1;
            }
        }
    }
    for (j = 0; a[j] < 0; j++)
        ;
    return j;
}

void test(int n, int k) {
    CHECK(josephus_rec(n, k) == josephus(n, k));
    auto j1 = josephus(n, k);
    auto j2 = josephus_simple(n, k);
    if (j1 != j2) {
        cout << "FAILED: n=" << n << " k=" << k << ": j1=" << j1 << " j2=" << j2
             << endl;
        exit(-1);
    }
}

int main() {
    test(3, 2);
    test(7, 2);
    test(7, 3);
    test(7, 4);
    test(6, 2);
    test(6, 3);
    test(1, 3);
    test(1, 2);
    cout << "TESTS PASSED." << endl;
    return 0;
}
