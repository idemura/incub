#include <algorithm>
#include <cmath>
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <deque>
#include <map>
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

namespace dot {
// D for directed (undirected by default)
// W for weighted
// Suffix L means adjacency list
//        M means matrix

std::string printGraphL(std::vector<std::vector<int>> const &g) {
    std::string s;
    s.append("strict graph {\n");
    for (size_t i = 0; i < g.size(); i++) {
        char buf[80];
        std::snprintf(buf, sizeof(buf), "\"%d\"[shape=circle]\n", (int)i);
        s.append(buf);
        for (auto v : g[i]) {
            std::snprintf(buf, sizeof(buf), "\"%d\"--\"%d\"\n", (int)i, v);
            s.append(buf);
        }
    }
    s.append("}\n");
    return s;
}

// Directed Weighted Graph
std::string printDWGraphM(std::vector<std::vector<int>> const &g) {
    std::string s;
    s.append("digraph {\n");
    for (size_t i = 0; i < g.size(); i++) {
        char buf[80];
        std::snprintf(buf, sizeof(buf), "\"%d\"[shape=circle]\n", (int)i);
        s.append(buf);
        for (size_t j = 0; j < g.size(); j++) {
            if (g[i][j] > 0) {
                std::snprintf(
                        buf,
                        sizeof(buf),
                        "\"%d\"->\"%d\"[label=\"%d\"]\n",
                        (int)i,
                        (int)j,
                        g[i][j]);
                s.append(buf);
            }
        }
    }
    s.append("}\n");
    return s;
}

void writeFile(char const *name, std::string const &s) {
    auto f = std::fopen(name, "wt");
    if (f) {
        std::fputs(s.c_str(), f);
        std::fclose(f);
    }
}
} // namespace dot

template <typename T, size_t N>
constexpr size_t countOf(T const (&a)[N]) {
    return N;
}

char const *nameStr(int v) {
    static char const *const Names[] = {
            "A", "B", "C", "D", "E", "F", "G", "H", "I", "J"};
    if (v < countOf(Names)) {
        return Names[v];
    }
    return "<out-of-range>";
}

void printMatrix(vector<vector<int>> const &m) {
    string header;
    for (int i = 0; i < m.size(); i++) {
        header.append(nameStr(i)).append(" ");
    }
    VLOG1 << "   " << header;
    for (int i = 0; i < m.size(); i++) {
        string s;
        for (auto a : m[i]) {
            s.append(to_string(a)).append(" ");
        }
        VLOG1 << nameStr(i) << ": " << s;
    }
}

// Constructs maximum flow in the graph
int flow(vector<vector<int>> &f, int s, int t, vector<vector<int>> &outf) {
    deque<int> q;
    vector<int> prev(f.size());
    outf.resize(f.size(), vector<int>(f.size()));
    for (;;) {
        prev.assign(prev.size(), -1);
        prev[s] = s;
        q.push_back(s);
        int u = s;
        while (!q.empty()) {
            u = q.front();
            q.pop_front();
            if (u == t) {
                break;
            }
            for (int i = 0; i < f[u].size(); i++) {
                if (f[u][i] > 0 && prev[i] < 0) {
                    prev[i] = u;
                    q.push_back(i);
                }
            }
        }
        if (u != t) {
            break;
        }
        q.clear();
        int flowOnPath = numeric_limits<int>::max();
        for (int v = t; v != s; v = prev[v]) {
            flowOnPath = std::min(flowOnPath, f[prev[v]][v]);
        }
        for (int v = t; v != s; v = prev[v]) {
            f[prev[v]][v] -= flowOnPath;
            f[v][prev[v]] += flowOnPath;
            outf[prev[v]][v] += flowOnPath;
        }
    }
    int sum = 0;
    for (int i = 0; i < f.size(); i++) {
        sum += f[t][i];
    }
    // Some vertex pairs can have flow forth and back. Eliminate these.
    for (int i = 0; i < f.size(); i++) {
        for (int j = 0; j < i; j++) {
            auto base = std::min(outf[i][j], outf[j][i]);
            outf[i][j] -= base;
            outf[j][i] -= base;
        }
    }
    return sum;
}

// See wikipedia for image
TEST(EdmondsKarp, Case1) {
    vector<vector<int>> m(7);
    //                 A  B  C  D  E  F  G
    m[0] = vector<int>{0, 3, 0, 3, 0, 0, 0}; // A
    m[1] = vector<int>{0, 0, 4, 0, 0, 0, 0}; // B
    m[2] = vector<int>{3, 0, 0, 1, 2, 0, 0}; // C
    m[3] = vector<int>{0, 0, 0, 0, 2, 6, 0}; // D
    m[4] = vector<int>{0, 1, 0, 0, 0, 0, 1}; // E
    m[5] = vector<int>{0, 0, 0, 0, 0, 0, 9}; // F
    m[6] = vector<int>{0, 0, 0, 0, 0, 0, 0}; // G
    vector<vector<int>> outf;
    EXPECT_EQ(5, flow(m, 0, 6, outf));
    vector<int> const v0{0, 2, 0, 3, 0, 0, 0};
    EXPECT_EQ(v0, outf[0]);
    vector<int> const v1{0, 0, 2, 0, 0, 0, 0};
    EXPECT_EQ(v1, outf[1]);
    vector<int> const v2{0, 0, 0, 1, 1, 0, 0};
    EXPECT_EQ(v2, outf[2]);
    vector<int> const v3{0, 0, 0, 0, 0, 4, 0};
    EXPECT_EQ(v3, outf[3]);
    vector<int> const v4{0, 0, 0, 0, 0, 0, 1};
    EXPECT_EQ(v4, outf[4]);
    vector<int> const v5{0, 0, 0, 0, 0, 0, 4};
    EXPECT_EQ(v5, outf[5]);
    vector<int> const v6{0, 0, 0, 0, 0, 0, 0};
    EXPECT_EQ(v6, outf[6]);
}

// Geeks for Geeks, Ford-Fulkerson
// This is one possible solution, G4G has another flow with same value
TEST(EdmondsKarp, Case2) {
    vector<vector<int>> m(6);
    //                  0   1   2   3   4   5
    m[0] = vector<int>{ 0, 16, 13,  0,  0,  0};
    m[1] = vector<int>{ 0,  0, 10, 12,  0,  0};
    m[2] = vector<int>{ 0,  4,  0,  0, 14,  0};
    m[3] = vector<int>{ 0,  0,  9,  0,  0, 20};
    m[4] = vector<int>{ 0,  0,  0,  7,  0,  4};
    m[5] = vector<int>{ 0,  0,  0,  0,  0,  0};
    vector<vector<int>> outf;
    EXPECT_EQ(23, flow(m, 0, 5, outf));
    vector<int> const v0{ 0, 12, 11,  0,  0,  0};
    EXPECT_EQ(v0, outf[0]);
    vector<int> const v1{ 0,  0,  0, 12,  0,  0};
    EXPECT_EQ(v1, outf[1]);
    vector<int> const v2{ 0,  0,  0,  0, 11,  0};
    EXPECT_EQ(v2, outf[2]);
    vector<int> const v3{ 0,  0,  0,  0,  0, 19};
    EXPECT_EQ(v3, outf[3]);
    vector<int> const v4{ 0,  0,  0,  7,  0,  4};
    EXPECT_EQ(v4, outf[4]);
    vector<int> const v5{ 0,  0,  0,  0,  0,  0};
    EXPECT_EQ(v5, outf[5]);
}

int main(int argc, char **argv) {
    initLog(argc, argv);
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
