#include <algorithm>
#include <vector>

#include "log.h"
#include <gtest/gtest.h>

using namespace std;

vector<int> totientTable(int n) {
    vector<int> t(n + 1);
    for (size_t i = 0; i < t.size(); i++) {
        t[i] = (int)i;
    }
    int i = 2;
    for (; i <= n; i++) {
        if (t[i] != i) {
            continue;
        }
        for (int j = i; j <= n; j += i) {
            t[j] -= t[j] / i;
        }
    }
    return t;
}

int totient(int n) {
    int t = n;
    for (int i = 2; i * i <= n; i++) {
        if (n % i == 0) {
            while (n % i == 0) {
                n /= i;
            }
            t -= t / i;
        }
    }
    if (n > 1) {
        t -= t / n;
    }
    return t;
}

TEST(Totient, Table) {
    const auto t = totientTable(18);
    EXPECT_EQ(19, t.size());
    EXPECT_EQ(0, t[0]);
    EXPECT_EQ(1, t[1]);
    EXPECT_EQ(1, t[2]);
    EXPECT_EQ(2, t[3]);
    EXPECT_EQ(2, t[4]);
    EXPECT_EQ(4, t[5]);
    EXPECT_EQ(2, t[6]);
    EXPECT_EQ(6, t[7]);
    EXPECT_EQ(4, t[8]);
    EXPECT_EQ(6, t[9]);
    EXPECT_EQ(4, t[10]);
    EXPECT_EQ(10, t[11]);
    EXPECT_EQ(4, t[12]);
    EXPECT_EQ(12, t[13]);
    EXPECT_EQ(6, t[14]);
    EXPECT_EQ(8, t[15]);
    EXPECT_EQ(8, t[16]);
    EXPECT_EQ(16, t[17]);
    EXPECT_EQ(6, t[18]);
}

TEST(Totient, Point) {
    EXPECT_EQ(0, totient(0));
    EXPECT_EQ(1, totient(1));
    EXPECT_EQ(1, totient(2));
    EXPECT_EQ(2, totient(3));
    EXPECT_EQ(2, totient(4));
    EXPECT_EQ(4, totient(5));
    EXPECT_EQ(2, totient(6));
    EXPECT_EQ(6, totient(7));
    EXPECT_EQ(4, totient(8));
    EXPECT_EQ(6, totient(9));
    EXPECT_EQ(4, totient(10));
    EXPECT_EQ(10, totient(11));
    EXPECT_EQ(4, totient(12));
    EXPECT_EQ(12, totient(13));
    EXPECT_EQ(6, totient(14));
    EXPECT_EQ(8, totient(15));
    EXPECT_EQ(8, totient(16));
    EXPECT_EQ(16, totient(17));
    EXPECT_EQ(6, totient(18));
}

int main(int argc, char **argv) {
    initLog(argc, argv);
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
