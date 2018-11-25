#include <algorithm>
#include <unordered_map>
#include <vector>

#include "log.h"
#include <gtest/gtest.h>

constexpr int PInf = 1'000'000;

struct TSResult {
    int length{PInf};
    std::vector<int> path;
};

struct Cache {
    int length{-1};
    int lastVertex{0};

    Cache() = default;
    Cache(int v, int l): length{l}, lastVertex{v} {}
};

std::string logTag(int last, int vSet) {
    std::string tag;
    tag.append("[last=").append(std::to_string(last)).append(" ");
    for (int i = 0; (1 << i) <= vSet; i++) {
        if (vSet & (1 << i)) {
            tag.append(std::to_string(i)).append(" ");
        }
    }
    if (tag.back() == ' ') {
        tag.pop_back();
    }
    tag.append("]");
    return tag;
}

void travelingSalesmanRec(
        std::vector<std::vector<int>> const &m,
        std::vector<std::vector<Cache>> &dp,
        int last,
        int vSet) {
    if (dp[vSet][last].length > 0) {
        return;
    }
    if (vSet == 0) {
        dp[vSet][last] = Cache{0, m[0][last]};
        return;
    }
    Cache c{0, PInf};
    for (int u = 0; u < m.size(); u++) {
        if ((vSet & (1 << u)) && m[last][u] < PInf) {
            int subset = vSet & ~(1 << u);
            travelingSalesmanRec(m, dp, u, subset);
            if (dp[subset][u].length == PInf) {
                continue;
            }
            int newLen = dp[subset][u].length + m[last][u];
            if (newLen < c.length) {
                c.length = newLen;
                c.lastVertex = u;
            }
        }
    }
    dp[vSet][last] = c;
}

// @m is matrix, eges length must be positive.
TSResult travelingSalesman(std::vector<std::vector<int>> const &m) {
    std::vector<std::vector<Cache>> dp(1 << m.size());
    for (auto &r : dp) {
        r.resize(m.size());
    }
    // @vSet contains all vertices except #0
    int vSet = ((1 << m.size()) - 1) & ~1;
    TSResult res;
    travelingSalesmanRec(m, dp, 0, vSet);
    res.length = dp[vSet][0].length;
    if (res.length != PInf) {
        res.path.push_back(0);
        int l = dp[vSet][0].lastVertex;
        while (l != 0) {
            res.path.push_back(l);
            vSet &= ~(1 << l);
            l = dp[vSet][l].lastVertex;
        }
        res.path.push_back(0);
    }
    return res;
}

TEST(TSP, Basic) {
    std::vector<std::vector<int>> m(4);
    m[0] = std::vector<int>{PInf, 10, 15, 20};
    m[1] = std::vector<int>{10, PInf, 35, 25};
    m[2] = std::vector<int>{15, 35, PInf, 30};
    m[3] = std::vector<int>{20, 25, 30, PInf};
    auto res = travelingSalesman(m);
    EXPECT_EQ(80, res.length);
    EXPECT_EQ(5, res.path.size());
    EXPECT_EQ(0, res.path[0]);
    EXPECT_EQ(1, res.path[1]);
    EXPECT_EQ(3, res.path[2]);
    EXPECT_EQ(2, res.path[3]);
    EXPECT_EQ(0, res.path[4]);
}

int main(int argc, char **argv) {
    initLog(argc, argv);
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
