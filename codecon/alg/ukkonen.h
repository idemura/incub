#ifndef UKKONEN_H
#define UKKONEN_H

#include <algorithm>
#include <cstdint>
#include <cstring>
#include <functional>
#include <string>
#include <utility>

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

    uint32_t length() const {
        return last - first;
    }

    uint32_t lengthTo(uint32_t i) const {
        return std::min(length(), i - first);
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
using FindSubstrResult = std::pair<bool, uint32_t>;

Node *build(char const *s, uint32_t sLen);
FindSubstrResult findSubstr(
        Node const *root, char const *s, char const *p, uint32_t pLen);
void destroy(Node *root);
void print(Node const *root, char const *s, uint32_t sLen, PrintFn const &fn);
} // namespace suffix_tree

#endif
