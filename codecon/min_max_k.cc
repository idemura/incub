// Given an array of ints A and int k find shortest subarray S=A[i..j] such that
// max(S) - min(S) = k.

#include <algorithm>
#include <cassert>
#include <deque>
#include <limits>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#include "log.h"
#include <gtest/gtest.h>

using namespace std;

using i32 = int32_t;
using i64 = int64_t;
using pii = std::pair<int, int>;

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

pair<int, int> findMinMaxKSorted(vector<int> const &a, int k) {
    DCHECK(is_sorted(a.begin(), a.end()));
    int i = 0;
    int j = a.size() - 1;
    if (a.size() <= 1 || a[j] - a[i] < k) {
        return pair<int, int>{0, 0};
    }

    constexpr auto Max = numeric_limits<int>::max();
    while (i < j) {
        int iNext = (a[j] - a[i + 1] >= k ? a[i + 1] - a[i] : Max);
        int jNext = (a[j - 1] - a[i] >= k ? a[j] - a[j - 1] : Max);
        if (iNext < jNext) {
            i++;
            continue;
        }
        if (jNext < iNext) {
            j--;
            continue;
        }
        // Equal here. Are both @Max, meaning to move can be done?
        if (iNext == Max) {
            break;
        }
        i++;
    }
    return pair<int, int>{i, j};
}

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

TEST(MinMaxK, Ordered) {
    vector<int> a{1, 2, 4, 6, 8};
    EXPECT_EQ(pii(2, 4), findMinMaxKSorted(a, 4));
}

int main(int argc, char **argv) {
    initLog(argc, argv);
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
