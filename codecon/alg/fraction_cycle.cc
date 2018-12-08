#include <string>
#include <unordered_map>
#include <vector>

#include "log.h"
#include <gtest/gtest.h>

using namespace std;

void appendInt(string &s, int n) {
    char buf[16];
    snprintf(buf, sizeof(buf), "%d", n);
    s.append(buf);
}

string fractionStr(int a, int b) {
    DCHECK_GT(b, 0);
    string s;
    appendInt(s, a / b);
    a %= b;
    if (a == 0) {
        return s;
    }
    s.append(".");
    unordered_map<int, int> rem;
    while (a != 0 && rem.find(a) == rem.end()) {
        rem[a] = (int)s.size();
        appendInt(s, (10 * a) / b);
        a = (10 * a) % b;
    }
    if (a != 0) {
        s.insert(rem[a], "(");
        s.append(")");
    }
    return s;
}

TEST(FractionStr, All) {
    EXPECT_EQ("2", fractionStr(4, 2));
    EXPECT_EQ("-2", fractionStr(-4, 2));
    EXPECT_EQ("0.5", fractionStr(2, 4));
    EXPECT_EQ("0.5", fractionStr(3, 6));
    EXPECT_EQ("0.25", fractionStr(1, 4));
    EXPECT_EQ("1.(3)", fractionStr(4, 3));
    EXPECT_EQ("0.(18)", fractionStr(2, 11));
    EXPECT_EQ("0.2(142857)", fractionStr(3, 14));
}

int main(int argc, char **argv) {
    initLog(argc, argv);
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
