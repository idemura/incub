#include "ukkonen.h"

#include <cstdio>
#include <iostream>
#include <unordered_map>
#include <utility>

#include <gtest/gtest.h>

namespace suffix_tree {
static std::string substr(char const *s, uint32_t first, uint32_t last) {
    DCHECK_GE(last, first);
    return std::string(s + first, last - first);
}

static uint32_t toIndex(char c) {
    if (c == EoLn) {
        return 0;
    }
    auto i = c - Base + 1; // Int
    DCHECK(1 <= i && i <= AlphabetSize) << "i " << i;
    return (uint32_t)i;
}

std::string Edge::name(char const *s) const {
    DCHECK(valid());
    return substr(s, first, last);
}

Edge Node::getEdge(char c) const {
    return e[toIndex(c)];
}

uint32_t Node::countEdges() const {
    uint32_t n = 0;
    for (uint32_t i = 0; i <= AlphabetSize; i++) {
        if (e[i].valid()) {
            n++;
        }
    }
    return n;
}

struct ActivePoint {
    explicit ActivePoint(Node *node): node{node} {}

    uint32_t getActiveChar() const {
        DCHECK_GT(length, 0);
        return node->e[edge].first + length;
    }

    Node *node{};
    uint32_t length{};
    uint32_t edge{}; // Undefined if @length == 0
};

// Ukkonen algorithm to build a suffix tree
Node *build(char const *s, uint32_t sLen) {
    // PrintFn vlogStr{[](std::string const &s) { VLOG(1) << s; }};
    auto root = new Node{};
    if (sLen == 0) {
        return root;
    }
    DCHECK_EQ(EoLn, s[sLen - 1]);
    uint32_t remainingFirst = 0;
    ActivePoint ap{root};
    for (uint32_t i = 0; i < sLen; i++) {
        // print(root, s, i, vlogStr);
        auto e = toIndex(s[i]);
        Node *prevSplit = nullptr;
        while (i >= remainingFirst) {
            // Try to move along the edge (or start a path if length == 0).
            auto al = ap.length;
            if (ap.length == 0) {
                if (ap.node->e[e].valid()) {
                    DCHECK_EQ(s[ap.node->e[e].first], s[i]);
                    ap.edge = e;
                    ap.length++;
                    DCHECK_EQ(1, ap.length);
                }
            } else if (s[ap.getActiveChar()] == s[i]) {
                ap.length++;
            }
            if (ap.length > al) {
                if (ap.length == ap.node->e[ap.edge].lengthTo(i) &&
                    ap.node->e[ap.edge].link) {
                    ap.length = 0;
                    ap.node = ap.node->e[ap.edge].link;
                    DCHECK_NOTNULL(ap.node);
                }
                break;
            }
            if (ap.length > 0) {
                // Split edge in the middle
                auto newNode = new Node{};
                newNode->suffixLink = root;
                newNode->e[e].first = i;
                newNode->e[e].last = sLen;
                auto ac = ap.getActiveChar();
                auto k = toIndex(s[ac]);
                newNode->e[k] = ap.node->e[ap.edge];
                newNode->e[k].first = ac;
                ap.node->e[ap.edge].link = newNode;
                ap.node->e[ap.edge].last = ac;
                if (prevSplit) {
                    prevSplit->suffixLink = newNode;
                }
                prevSplit = newNode;
            } else {
                // Make a leaf edge
                DCHECK(!ap.node->e[e].valid());
                ap.node->e[e].first = i;
                ap.node->e[e].last = sLen;
            }
            remainingFirst++;
            if (ap.node != root) {
                ap.node = ap.node->suffixLink;
            } else if (ap.length > 0) {
                DCHECK_LT(remainingFirst, sLen);
                DCHECK_LT(i, sLen);
                ap.length--;
                ap.edge = toIndex(s[remainingFirst]);
            }
            // print(root, s, i + 1, vlogStr);
        }
    }
    DCHECK_EQ(remainingFirst, sLen);
    // print(root, s, sLen, vlogStr);
    return root;
}

void destroy(Node *root) {
    if (root == nullptr) {
        return;
    }
    for (uint32_t i = 0; i < AlphabetSize; i++) {
        destroy(root->e[i].link);
    }
}

static void printRec(
        Node const *node,
        char const *s,
        uint32_t sLen,
        std::string indent,
        PrintFn const &fn) {
    static const char TabStep[] = "  ";
    if (node == nullptr) {
        return;
    }
    char buf[80];
    std::snprintf(
            buf, sizeof(buf), "Node %p suffixLink %p", node, node->suffixLink);
    fn(indent + buf);
    for (uint32_t i = 0; i <= AlphabetSize; i++) {
        auto e = node->e[i];
        if (!e.valid()) {
            continue;
        }
        auto c = i == 0 ? EoLn : char(Base + i - 1);
        auto trueLast = std::min(e.last, sLen);
        std::snprintf(
                buf,
                sizeof(buf),
                "Edge '%c' [%i, %i] %s link %p",
                c,
                e.first,
                trueLast,
                substr(s, e.first, trueLast).c_str(),
                e.link);
        fn(indent + buf);
        printRec(e.link, s, sLen, indent + TabStep, fn);
    }
    std::snprintf(buf, sizeof(buf), "Done %p", node);
    fn(indent + buf);
}

void print(Node const *root, char const *s, uint32_t len, PrintFn const &fn) {
    printRec(root, s, len, "", fn);
}
} // namespace suffix_tree

using suffix_tree::AlphabetSize;
using suffix_tree::Edge;
using suffix_tree::Node;

class IdMap {
public:
    explicit IdMap(Node const *root): id{1} {
        buildRec(root);
    }

    uint32_t getId(Node const *node) const {
        auto iter = idMap.find(node);
        return iter == idMap.end() ? 0 : iter->second;
    }

    size_t size() const {
        return idMap.size();
    }

private:
    // DFS, prints current node first
    void buildRec(Node const *node) {
        if (node != nullptr) {
            idMap[node] = id++;
            for (uint32_t i = 0; i <= AlphabetSize; i++) {
                buildRec(node->e[i].link);
            }
        }
    }

    uint32_t id;
    std::unordered_map<Node const *, uint32_t> idMap;
};

uint32_t validEdgeCount(Node const *node) {
    uint32_t res = 0;
    for (uint32_t i = 0; i <= AlphabetSize; i++) {
        if (node->e[i].valid()) {
            res++;
        }
    }
    return res;
}

suffix_tree::Node *build(char const *s) {
    std::cout << "Input " << s << std::endl;
    auto sLen = (uint32_t)std::strlen(s);
    auto root = suffix_tree::build(s, sLen);
    suffix_tree::print(root, s, sLen, [](std::string const &s) {
        std::cout << s << "\n";
    });
    return root;
}

#define GET_EDGE(NODE, STR, NAME)                                              \
    auto e = NODE->getEdge(NAME[0]);                                           \
    EXPECT_EQ(NAME, e.name(STR));

#define CHECK_LEAF_EDGE(NODE, STR, NAME)                                       \
    do {                                                                       \
        GET_EDGE(NODE, STR, NAME);                                             \
        EXPECT_TRUE(e.isLeafEdge());                                           \
        EXPECT_TRUE(e.link == nullptr);                                        \
    } while (false)

#define CHECK_INTERNAL_EDGE(NODE, STR, NAME)                                   \
    do {                                                                       \
        GET_EDGE(NODE, STR, NAME);                                             \
        EXPECT_FALSE(e.isLeafEdge());                                          \
    } while (false)

TEST(Ukkonen, Case1) {
    char const str[] = "xyx$";
    auto root = build(str);
    IdMap map{root};
    EXPECT_EQ(2, map.size());
    {
        auto node = root;
        EXPECT_EQ(3, node->countEdges());
        EXPECT_EQ(1, map.getId(node));
        EXPECT_EQ(0, map.getId(node->suffixLink));

        CHECK_LEAF_EDGE(node, str, "$");
        CHECK_INTERNAL_EDGE(node, str, "x");
        CHECK_LEAF_EDGE(node, str, "yx$");
    }
    {
        auto node = root->getEdge('x').link;
        EXPECT_EQ(2, node->countEdges());
        EXPECT_EQ(2, map.getId(node));
        EXPECT_EQ(1, map.getId(node->suffixLink));

        CHECK_LEAF_EDGE(node, str, "$");
        CHECK_LEAF_EDGE(node, str, "yx$");
    }
    suffix_tree::destroy(root);
}

TEST(Ukkonen, Case2) {
    char const str[] = "xaxbxc$";
    auto root = build(str);
    IdMap map{root};
    EXPECT_EQ(2, map.size());
    {
        auto node = root;
        EXPECT_EQ(5, node->countEdges());
        EXPECT_EQ(1, map.getId(node));
        EXPECT_EQ(0, map.getId(node->suffixLink));

        CHECK_LEAF_EDGE(node, str, "$");
        CHECK_LEAF_EDGE(node, str, "axbxc$");
        CHECK_LEAF_EDGE(node, str, "bxc$");
        CHECK_LEAF_EDGE(node, str, "c$");
        CHECK_INTERNAL_EDGE(node, str, "x");
    }
    {
        auto node = root->getEdge('x').link;
        EXPECT_EQ(3, node->countEdges());
        EXPECT_EQ(2, map.getId(node));
        EXPECT_EQ(1, map.getId(node->suffixLink));

        CHECK_LEAF_EDGE(node, str, "axbxc$");
        CHECK_LEAF_EDGE(node, str, "bxc$");
        CHECK_LEAF_EDGE(node, str, "c$");
    }
    suffix_tree::destroy(root);
}

TEST(Ukkonen, Case3) {
    char const str[] = "xyzxyaxyz$";
    auto root = build(str);
    IdMap map{root};
    EXPECT_EQ(6, map.size());
    {
        auto node = root;
        EXPECT_EQ(5, node->countEdges());
        EXPECT_EQ(1, map.getId(node));
        EXPECT_EQ(0, map.getId(node->suffixLink));

        CHECK_LEAF_EDGE(node, str, "$");
        CHECK_LEAF_EDGE(node, str, "axyz$");
        CHECK_INTERNAL_EDGE(node, str, "xy");
        CHECK_INTERNAL_EDGE(node, str, "y");
        CHECK_INTERNAL_EDGE(node, str, "z");
    }
    {
        auto node = root->getEdge('x').link;
        EXPECT_EQ(2, node->countEdges());
        EXPECT_EQ(2, map.getId(node));
        EXPECT_EQ(4, map.getId(node->suffixLink));

        CHECK_LEAF_EDGE(node, str, "axyz$");
        CHECK_INTERNAL_EDGE(node, str, "z");
    }
    {
        auto node = root->getEdge('x').link->getEdge('z').link;
        EXPECT_EQ(2, node->countEdges());
        EXPECT_EQ(3, map.getId(node));
        EXPECT_EQ(5, map.getId(node->suffixLink));

        CHECK_LEAF_EDGE(node, str, "$");
        CHECK_LEAF_EDGE(node, str, "xyaxyz$");
    }
    {
        auto node = root->getEdge('y').link;
        EXPECT_EQ(2, node->countEdges());
        EXPECT_EQ(4, map.getId(node));
        EXPECT_EQ(1, map.getId(node->suffixLink));

        CHECK_LEAF_EDGE(node, str, "axyz$");
        CHECK_INTERNAL_EDGE(node, str, "z");
    }
    {
        auto node = root->getEdge('y').link->getEdge('z').link;
        EXPECT_EQ(2, node->countEdges());
        EXPECT_EQ(5, map.getId(node));
        EXPECT_EQ(6, map.getId(node->suffixLink));

        CHECK_LEAF_EDGE(node, str, "$");
        CHECK_LEAF_EDGE(node, str, "xyaxyz$");
    }
    {
        auto node = root->getEdge('z').link;
        EXPECT_EQ(2, node->countEdges());
        EXPECT_EQ(6, map.getId(node));
        EXPECT_EQ(1, map.getId(node->suffixLink));

        CHECK_LEAF_EDGE(node, str, "$");
        CHECK_LEAF_EDGE(node, str, "xyaxyz$");
    }
    suffix_tree::destroy(root);
}

TEST(Ukkonen, Case4) {
    char const str[] = "xxxx$";
    auto root = build(str);
    IdMap map{root};
    EXPECT_EQ(4, map.size());
    {
        auto node = root;
        EXPECT_EQ(2, node->countEdges());
        EXPECT_EQ(1, map.getId(node));
        EXPECT_EQ(0, map.getId(node->suffixLink));

        CHECK_LEAF_EDGE(node, str, "$");
        CHECK_INTERNAL_EDGE(node, str, "x");
    }
    {
        auto node = root->getEdge('x').link;
        EXPECT_EQ(2, node->countEdges());
        EXPECT_EQ(2, map.getId(node));
        EXPECT_EQ(1, map.getId(node->suffixLink));

        CHECK_LEAF_EDGE(node, str, "$");
        CHECK_INTERNAL_EDGE(node, str, "x");
    }
    {
        auto node = root->getEdge('x').link->getEdge('x').link;
        EXPECT_EQ(2, node->countEdges());
        EXPECT_EQ(3, map.getId(node));
        EXPECT_EQ(2, map.getId(node->suffixLink));

        CHECK_LEAF_EDGE(node, str, "$");
        CHECK_INTERNAL_EDGE(node, str, "x");
    }
    {
        auto node =
                root->getEdge('x').link->getEdge('x').link->getEdge('x').link;
        EXPECT_EQ(2, node->countEdges());
        EXPECT_EQ(4, map.getId(node));
        EXPECT_EQ(3, map.getId(node->suffixLink));

        CHECK_LEAF_EDGE(node, str, "$");
        CHECK_LEAF_EDGE(node, str, "x$");
    }
}

TEST(Ukkonen, Case5) {
    char const str[] = "abxabyabz$";
    auto root = build(str);
    IdMap map{root};
    EXPECT_EQ(3, map.size());
    {
        auto node = root;
        EXPECT_EQ(6, node->countEdges());
        EXPECT_EQ(1, map.getId(node));
        EXPECT_EQ(0, map.getId(node->suffixLink));

        CHECK_LEAF_EDGE(node, str, "$");
        CHECK_INTERNAL_EDGE(node, str, "ab");
        CHECK_INTERNAL_EDGE(node, str, "b");
        CHECK_LEAF_EDGE(node, str, "xabyabz$");
        CHECK_LEAF_EDGE(node, str, "yabz$");
        CHECK_LEAF_EDGE(node, str, "z$");
    }
    {
        auto node = root->getEdge('a').link;
        EXPECT_EQ(3, node->countEdges());
        EXPECT_EQ(2, map.getId(node));
        EXPECT_EQ(3, map.getId(node->suffixLink));

        CHECK_LEAF_EDGE(node, str, "xabyabz$");
        CHECK_LEAF_EDGE(node, str, "yabz$");
        CHECK_LEAF_EDGE(node, str, "z$");
    }
    {
        auto node = root->getEdge('b').link;
        EXPECT_EQ(3, node->countEdges());
        EXPECT_EQ(3, map.getId(node));
        EXPECT_EQ(1, map.getId(node->suffixLink));

        CHECK_LEAF_EDGE(node, str, "xabyabz$");
        CHECK_LEAF_EDGE(node, str, "yabz$");
        CHECK_LEAF_EDGE(node, str, "z$");
    }
}

int main(int argc, char **argv) {
    initLog(argc, argv);
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
