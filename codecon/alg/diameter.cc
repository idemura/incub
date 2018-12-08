#include <algorithm>
#include <utility>
#include <vector>

#include "log.h"
#include <gtest/gtest.h>

using namespace std;

// Output: .first - diameter, .second - max distance to leaf
pair<int, int> diameterRec(vector<vector<int>> const &m, int v, int parent) {
    int maxD = 0;
    int top2Len[2]{};
    for (int i = 0; i < m[v].size(); i++) {
        if (m[v][i] != parent) {
            auto r = diameterRec(m, m[v][i], v);
            if (r.second >= top2Len[0]) {
                top2Len[1] = top2Len[0];
                top2Len[0] = r.second;
            } else if (r.second > top2Len[1]) {
                top2Len[1] = r.second;
            }
            maxD = std::max(maxD, r.first);
        }
    }
    if (top2Len[0] == 0) {
        return {0, 1};
    }
    int d = (top2Len[1] == 0 ? top2Len[0] : top2Len[0] + top2Len[1]);
    return make_pair(std::max(maxD, d), top2Len[0] + 1);
}

int diameter(vector<vector<int>> const &m) {
    if (m[0].size() == 0) {
        return 0;
    }
    return diameterRec(m, 0, -1).first;
}

TEST(Diameter, Case1) {
    vector<vector<int>> m(1);
    EXPECT_EQ(0, diameter(m));
}

TEST(Diameter, Case2) {
    vector<vector<int>> m(2);
    m[0] = vector<int>{1};
    m[1] = vector<int>{0};
    EXPECT_EQ(1, diameter(m));
}

TEST(Diameter, Case3) {
    vector<vector<int>> m(3);
    m[0] = vector<int>{1, 2};
    m[1] = vector<int>{0};
    m[2] = vector<int>{0};
    EXPECT_EQ(2, diameter(m));
}

TEST(Diameter, Case4) {
    //     0
    //     |
    // 1-2-3-4-5
    //     |
    //     6
    vector<vector<int>> m(7);
    m[0] = vector<int>{3};
    m[1] = vector<int>{2};
    m[2] = vector<int>{1, 3};
    m[3] = vector<int>{0, 2, 4, 6};
    m[4] = vector<int>{3, 5};
    m[5] = vector<int>{4};
    m[6] = vector<int>{3};
    EXPECT_EQ(4, diameter(m));
}
TEST(Diameter, Case5) {
    //     3
    //     |
    // 1-2-0-4-5
    //     |
    //     6
    vector<vector<int>> m(7);
    m[0] = vector<int>{2, 3, 4, 6};
    m[1] = vector<int>{2};
    m[2] = vector<int>{0, 1};
    m[3] = vector<int>{0};
    m[4] = vector<int>{0, 5};
    m[5] = vector<int>{4};
    m[6] = vector<int>{0};
    EXPECT_EQ(4, diameter(m));
}

int main(int argc, char **argv) {
    initLog(argc, argv);
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
