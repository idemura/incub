#include <algorithm>
#include <string>
#include <utility>
#include <vector>

#include "log.h"
#include <gtest/gtest.h>

using namespace std;

vector<int> zAlg(string const &s) {
    vector<int> z(s.size());
    z[0] = (int)s.size();
    int l = 0; // First
    int r = 0; // Last, exclusive
    for (int i = 1; i < s.size(); i++) {
        if (i < r && z[i - l] < r - i) {
            z[i] = z[i - l];
        } else {
            l = i;
            r = max(r, i);
            while (r < s.size() && s[r] == s[r - l]) {
                r++;
            }
            z[i] = r - i;
        }
    }
    return z;
}

TEST(ZAlg, Case1) {
    auto z = zAlg("aaaa");
    EXPECT_EQ(4, z.size());
    EXPECT_EQ(4, z[0]);
    EXPECT_EQ(3, z[1]);
    EXPECT_EQ(2, z[2]);
    EXPECT_EQ(1, z[3]);
}

TEST(ZAlg, Case2) {
    auto z = zAlg("ababcaba");
    EXPECT_EQ(8, z.size());
    EXPECT_EQ(8, z[0]);
    EXPECT_EQ(0, z[1]);
    EXPECT_EQ(2, z[2]);
    EXPECT_EQ(0, z[3]);
    EXPECT_EQ(0, z[4]);
    EXPECT_EQ(3, z[5]);
    EXPECT_EQ(0, z[6]);
    EXPECT_EQ(1, z[7]);
}

TEST(ZAlg, Case3) {
    auto z = zAlg("ababab");
    EXPECT_EQ(6, z.size());
    EXPECT_EQ(6, z[0]);
    EXPECT_EQ(0, z[1]);
    EXPECT_EQ(4, z[2]);
    EXPECT_EQ(0, z[3]);
    EXPECT_EQ(2, z[4]);
    EXPECT_EQ(0, z[5]);
}

TEST(ZAlg, Case4) {
    auto z = zAlg("aabcaabxaaaz");
    EXPECT_EQ(12, z.size());
    EXPECT_EQ(12, z[0]);
    EXPECT_EQ(1, z[1]);
    EXPECT_EQ(0, z[2]);
    EXPECT_EQ(0, z[3]);
    EXPECT_EQ(3, z[4]);
    EXPECT_EQ(1, z[5]);
    EXPECT_EQ(0, z[6]);
    EXPECT_EQ(0, z[7]);
    EXPECT_EQ(2, z[8]);
    EXPECT_EQ(2, z[9]);
    EXPECT_EQ(1, z[10]);
    EXPECT_EQ(0, z[11]);
}

TEST(ZAlg, Case5) {
    auto z = zAlg("abxabcabxabxabc");
    EXPECT_EQ(15, z.size());
    EXPECT_EQ(15, z[0]);
    EXPECT_EQ(0, z[1]);
    EXPECT_EQ(0, z[2]);
    EXPECT_EQ(2, z[3]);
    EXPECT_EQ(0, z[4]);
    EXPECT_EQ(0, z[5]);
    EXPECT_EQ(5, z[6]);
    EXPECT_EQ(0, z[7]);
    EXPECT_EQ(0, z[8]);
    EXPECT_EQ(6, z[9]);
    EXPECT_EQ(0, z[10]);
    EXPECT_EQ(0, z[11]);
    EXPECT_EQ(2, z[12]);
    EXPECT_EQ(0, z[13]);
    EXPECT_EQ(0, z[14]);
}

int main(int argc, char **argv) {
    initLog(argc, argv);
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
