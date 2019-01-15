// Given an array of integers a and int k find shortest subarray S=A[i..j] such
// that max(S) - min(S) >= k.

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

pair<int, int> findSubarrayKSorted(vector<int> const &a, int k) {
    DCHECK(is_sorted(a.begin(), a.end()));
    int i = 0;
    int j = (int)a.size() - 1;
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

template <typename T>
string vectorToString(vector<T> const &a) {
    string s;
    for (auto x : a) {
        s.append(to_string(x)).append(" ");
    }
    return s;
}

/*
pair<int, int> findSubarrayKNaive(vector<int> const &a, int k) {
    constexpr auto Max = numeric_limits<int>::max();
    pair<int, int> res{0, Max};
    for (int i = 0; i < a.size(); i++) {
        for (int j = i + 1; j < a.size(); j++) {
            if (abs(a[j] - a[i]) < k) {
                continue;
            }
            auto holds = false;
            auto rMin = min(a[i], a[j]);
            auto rMax = max(a[i], a[j]);
            for (int k = i; k <= j; k++) {
                if (a[k] < rMin || a[k] > rMax) break;
            }
            if (holds && j - i < res.second) {
                res = pair<int, int>{i, j - i};
            }
        }
    }
    if (res.second == Max) {
        res.second = 0;
    }
    return {res.first, res.first + res.second};
}
*/

pair<int, int> findSubarrayKHelper(vector<int> const &a, int k) {
    // This only works if min index is less than max index. Run on reverse array
    // to handle the reverse case.
    //
    // Stack stores positions in array a. We keep folowing invariant:
    // a[stack[j + 1]] > a[stack[j]].
    //
    // Let j = 0 position in the stack since which we keep looking, that we move
    // only right, trying to find shortest subarray.
    //
    // If a[i] >= top(stack), then we pop stack until invariant restored.
    // After while loop a[i] > top(stack) or stack is empty.
    //
    // Adjust j to be in stack bounds. Don't move it anywhere: we've seen items
    // more than a[i] (because a[i] <= a[i - 1]) and hence no lower item would
    // make a smaller subarray.

    if (a.empty()) {
        return {0, 0};
    }
    constexpr auto Max = numeric_limits<int>::max();
    int j = 0;
    int minLen = Max;
    int minPos = 0;
    vector<int> stack{0};
    DCHECK_EQ(1, stack.size());
    for (int i = 1; i < a.size(); i++) {
        // Keep invariant
        while (!stack.empty() && a[i] <= a[stack.back()]) {
            stack.pop_back();
        }
        // Adjust j to stack bounds
        if (stack.empty()) {
            j = 0;
        } else {
            j = min(j, (int)stack.size() - 1);
        }
        // Try advance j and update result
        while (j < stack.size() && a[i] - a[stack[j]] >= k) {
            if (i - stack[j] < minLen) {
                minLen = i - stack[j];
                minPos = stack[j];
            }
            j++;
        }
        stack.push_back(i);
    }
    return minLen == Max ? pair<int, int>{0, 0}
                         : pair<int, int>{minPos, minPos + minLen};
}

pair<int, int> findSubarrayK(vector<int> a, int k) {
    if (a.size() <= 1) {
        return {0, 0};
    }
    auto d = findSubarrayKHelper(a, k);
    reverse(a.begin(), a.end());
    auto r = findSubarrayKHelper(a, k);
    if (r.second - r.first > d.second - d.first) {
        d = r;
    }
    return d;
}

TEST(FindToRight, Case1) {
    vector<int> a{1, 2, 6, 3, 2, 5, 7};
    ClosestRightState crs{a};
    EXPECT_EQ(1, crs.findToRight(2, 0));
    EXPECT_EQ(1, crs.findToRight(2, 1));
    EXPECT_EQ(4, crs.findToRight(2, 2));
    EXPECT_EQ(4, crs.findToRight(2, 3));
    EXPECT_EQ(4, crs.findToRight(2, 4));
    EXPECT_EQ(-1, crs.findToRight(2, 5));
}

TEST(SubarrayK, HelperCase1) {
    vector<int> a{1, 2, 6, 3, 4, 5, 8};
    EXPECT_EQ(pii(1, 2), findSubarrayKHelper(a, 4));
    EXPECT_EQ(pii(0, 2), findSubarrayKHelper(a, 5));
    EXPECT_EQ(pii(1, 2), findSubarrayKHelper(a, 2));
    EXPECT_EQ(pii(0, 6), findSubarrayKHelper(a, 7));
}

TEST(SubarrayK, HelperCase2) {
    vector<int> a{1, 2, 6, 3, 8, 5, 9};
    EXPECT_EQ(pii(0, 4), findSubarrayKHelper(a, 7));
    EXPECT_EQ(pii(3, 4), findSubarrayKHelper(a, 5));
}

TEST(SubarrayK, HelperCase3) {
    vector<int> a{1, 2, 4, 7, 11};
    EXPECT_EQ(pii(0, 1), findSubarrayKHelper(a, 1));
    EXPECT_EQ(pii(2, 4), findSubarrayKHelper(a, 6));
    EXPECT_EQ(pii(1, 3), findSubarrayKHelper(a, 5));
}

TEST(SubarrayK, HelperCase4) {
    vector<int> a{1, 2, 4, 7, 11};
    EXPECT_EQ(pii(0, 1), findSubarrayKHelper(a, 1));
    EXPECT_EQ(pii(2, 4), findSubarrayKHelper(a, 6));
    EXPECT_EQ(pii(1, 3), findSubarrayKHelper(a, 5));
}

TEST(SubarrayK, HelperCase5) {
    vector<int> a{11, 7, 4, 2, 1};
    EXPECT_EQ(pii(0, 0), findSubarrayKHelper(a, 1));
    EXPECT_EQ(pii(0, 0), findSubarrayKHelper(a, 6));
    EXPECT_EQ(pii(0, 0), findSubarrayKHelper(a, 5));
}

TEST(SubarrayK, Case1) {
    vector<int> a{11, 7, 4, 2, 1};
    EXPECT_EQ(pii(0, 1), findSubarrayK(a, 1));
    EXPECT_EQ(pii(2, 4), findSubarrayK(a, 6));
    EXPECT_EQ(pii(1, 3), findSubarrayK(a, 5));
}

TEST(SubarrayK, Ordered) {
    vector<int> a{1, 2, 4, 6, 8};
    EXPECT_EQ(pii(2, 4), findSubarrayKSorted(a, 4));
}

int main(int argc, char **argv) {
    initLog(argc, argv);
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
