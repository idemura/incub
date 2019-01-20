#include <cstdio>
#include <string>
#include <vector>

#include "log.h"
#include <gtest/gtest.h>

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

using namespace std;

TEST(Dot, PrintGraphL) {
    vector<vector<int>> g(4);
    g[0] = vector<int>{1, 3};
    g[1] = vector<int>{0, 2, 3};
    g[2] = vector<int>{1};
    g[3] = vector<int>{0, 1};
    dot::writeFile("g10.dot", dot::printGraphL(g));
}

TEST(Dot, PrintDWGraphM_1) {
    vector<vector<int>> g(3);
    g[0] = vector<int>{0, 1, 3};
    g[1] = vector<int>{2, 0, 0};
    g[2] = vector<int>{0, 2, 0};
    dot::writeFile("g20.dot", dot::printDWGraphM(g));
}

int main(int argc, char **argv) {
    initLog(argc, argv);
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
