// Given an array of ints A and int k find shortest subarray S=A[i..j] such that
// max(S) - min(S) = k.

#include <algorithm>
#include <utility>
#include <vector>
#include <deque>
#include <unordered_map>
#include <string>

#include "log.h"
#include <gtest/gtest.h>

using namespace std;

struct ClosestRightState {
    unordered_map<int, vector<int>> invIdx;

    explicit ClosestRightState(vector<int> const &a) {
        assert(invIdx.empty());
        for (int i = 0; i < a.size(); i++) {
            invIdx[a[i]].push_back(i);
        }
    }

    int findToRight(int x, int from) const {
        auto itr = invIdx.find(x);
        if (itr == invIdx.end()) {
            return -1;
        }
        auto const &pl = itr->second;
        string s;
        for (auto a : pl) {
            s.append(to_string(a)).append(" ");
        }
        auto pos = lower_bound(pl.begin(), pl.end(), from);
        if (pos == pl.end()) {
            return -1;
        }
        return *pos;
    }
};

pair<int, int> findMinMaxK(vector<int> const &a, int k) {
    return {0, a.size()};
}

TEST(MinMaxK, Case1) {
    vector<int> a{1, 2, 6, 3, 2, 5, 7};
    ClosestRightState crs{a};
    EXPECT_EQ(1, crs.findToRight(2, 0));
    EXPECT_EQ(1, crs.findToRight(2, 1));
    EXPECT_EQ(4, crs.findToRight(2, 2));
    EXPECT_EQ(4, crs.findToRight(2, 3));
    EXPECT_EQ(4, crs.findToRight(2, 4));
    EXPECT_EQ(-1, crs.findToRight(2, 5));
}

int main(int argc, char **argv) {
    initLog(argc, argv);
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
