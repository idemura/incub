// Given an array of ints A and int k find shortest subarray S=A[i..j] such that
// max(S) - min(S) = k.

#include <algorithm>
#include <utility>
#include <vector>

#include "log.h"
#include <gtest/gtest.h>

using namespace std;

pair<int, int> findMinMaxK(vector<int> const &a, int k) {
    pair<int, int> res{0, a.size()};
    vector<int> stack;
    stack.push_back(0);
    for (int i = 1; i < a.size(); i++) {
    }
    return res;
}

TEST(MinMaxK, Case1) {
    auto res = findMinMaxK(vector<int>{1, 2, 6, 3, 5, 7}, 3);
}

int main(int argc, char **argv) {
    initLog(argc, argv);
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
