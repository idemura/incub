#ifndef UKKONEN_H
#define UKKONEN_H

#include <algorithm>
#include <cstdint>
#include <cstring>
#include <functional>
#include <string>

#include "log.h"

namespace suffix_tree {
constexpr uint32_t AlphabetSize = 26;
constexpr char Base = 'a';
constexpr char EoLn = '$';

struct Node;

struct Edge {
    Node *link{nullptr};
    uint32_t first{};
    uint32_t last{};

    uint32_t lengthTo(uint32_t i) const {
        return std::min(i, last) - first;
    }

    std::string name(char const *s) const;

    bool valid() const {
        return last > first;
    }

    bool isLeafEdge() const {
        return link == nullptr;
    }
};

struct Node {
    Node *suffixLink = nullptr;
    Edge e[AlphabetSize + 1]{};

    Edge getEdge(char c) const;
    uint32_t countEdges() const;
};

using PrintFn = std::function<void(std::string const &)>;

Node *build(char const *s, uint32_t sLen);
void destroy(Node *root);
void print(Node const *root, char const *s, uint32_t sLen, PrintFn const &fn);
} // namespace suffix_tree

#endif
