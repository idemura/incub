#ifndef UKKONEN_H
#define UKKONEN_H

#include <algorithm>
#include <cstdint>
#include <string>
#include <unordered_map>

namespace suffix_tree {
using i32 = int;
using u32 = unsigned int;

constexpr int AlphabetSize = 26;
constexpr char Base = 'a';
constexpr char EoLn = '$';

struct Node;

struct Edge {
    Node *link = nullptr;
    int first = -1;
    int last = -1;

    int lengthTo(int i) const {
        return std::min(i, last) - first;
    }

    std::string name(char const *s) const;

    bool valid() const {
        return first >= 0;
    }

    bool isLeafEdge() const {
        return link == nullptr;
    }
};

struct Node {
    Node *suffixLink = nullptr;
    Edge e[AlphabetSize + 1]{};

    Edge getEdge(char c) const;
    int countEdges() const;
};

using PrintFn = std::function<void(std::string const &)>;

int toIndex(int c);
Node *build(char const *s, int sLen);
void destroy(Node *root);
void print(Node const *root, char const *s, int sLen, PrintFn const &fn);
} // namespace suffix_tree

#endif
