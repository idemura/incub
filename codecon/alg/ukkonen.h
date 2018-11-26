#ifndef UKKONEN_H
#define UKKONEN_H

#include <algorithm>
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <functional>
#include <map>
#include <string>
#include <utility>

#include "log.h"

namespace suffix_tree {
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

    bool empty() const {
        return first == last;
    }

    std::string name(char const *s) const;
};

struct Node {
    Node *suffixLink{nullptr};
    std::map<char, Edge> e; // Order is important for tests

    Edge const *getEdge(char c) const;
    Edge *getEdgeInsert(char c);

    uint32_t numEdges() const;
};

using PrintFn = std::function<void(std::string const &)>;
using FindResult = std::pair<bool, uint32_t>;

constexpr FindResult NotFound{false, 0};

Node *build(char const *s, uint32_t sLen);
FindResult
findSubstr(Node const *root, char const *s, char const *p, uint32_t pLen);
void destroy(Node *root);
void print(Node const *root, char const *s, uint32_t sLen, PrintFn const &fn);
} // namespace suffix_tree

#endif
