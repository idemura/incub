#include "base.h"

vector<i64> totient(int n) {
    vector<i64> tot(n + 1);
    for (i64 i = 1; i <= n; i++) {
        tot[i] = i;
    }
    for (i64 i = 2; i <= n; i++) {
        if (tot[i] != i)
            continue;
        // Compared to sieve, can't start with i * i.
        for (i64 j = i; j <= n; j += i) {
            tot[j] = (tot[j] / i) * (i - 1);
        }
    }
    return tot;
}

int main() {
    const auto tot = totient(18);
    CHECK(tot.size() == 19);
    CHECK(tot[0] == 0);
    CHECK(tot[1] == 1);
    CHECK(tot[2] == 1);
    CHECK(tot[3] == 2);
    CHECK(tot[4] == 2);
    CHECK(tot[5] == 4);
    CHECK(tot[6] == 2);
    CHECK(tot[7] == 6);
    CHECK(tot[8] == 4);
    CHECK(tot[9] == 6);
    CHECK(tot[10] == 4);
    CHECK(tot[11] == 10);
    CHECK(tot[12] == 4);
    CHECK(tot[13] == 12);
    CHECK(tot[14] == 6);
    CHECK(tot[15] == 8);
    CHECK(tot[16] == 8);
    CHECK(tot[17] == 16);
    CHECK(tot[18] == 6);
    cout << "TESTS PASSED." << endl;
    return 0;
}
