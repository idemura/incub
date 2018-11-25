#include "ukkonen.h"

#include <iostream>
#include <unordered_map>

#include <gtest/gtest.h>

namespace suffix_tree {
static std::string substr(char const *s, uint32_t first, uint32_t last) {
    DCHECK_GE(last, first);
    return std::string(s + first, last - first);
}

std::string Edge::name(char const *s) const {
    DCHECK(!empty());
    return substr(s, first, last);
}

Edge const *Node::getEdge(char c) const {
    auto itr = e.find(c);
    if (itr == e.end()) {
        return nullptr;
    } else {
        return &itr->second;
    }
}

Edge *Node::getEdgeInsert(char c) {
    return &e.emplace(c, Edge{}).first->second;
}

uint32_t Node::numEdges() const {
    return (uint32_t)e.size();
}

struct ActivePoint {
    explicit ActivePoint(Node *node): node{node} {}

    uint32_t getStrPos() const {
        DCHECK_GT(length, 0);
        return edge->first + length;
    }

    Node *node{};
    uint32_t length{};
    Edge *edge{};
};

// Ukkonen algorithm to build a suffix tree
Node *build(char const *s, uint32_t sLen) {
    auto root = new Node{};
    if (sLen == 0) {
        return root;
    }
    uint32_t remainingFirst = 0;
    ActivePoint ap{root};
    for (uint32_t i = 0; i < sLen; i++) {
        Node *prevSplit = nullptr;
        while (i >= remainingFirst) {
            // Try to move along the edge (or start a path if length == 0).
            auto al = ap.length;
            if (ap.length == 0 || ap.edge == nullptr) {
                ap.edge = ap.node->getEdgeInsert(s[i]);
                if (!ap.edge->empty()) {
                    ap.length++;
                    DCHECK_EQ(s[ap.edge->first], s[i]);
                }
            } else if (s[ap.getStrPos()] == s[i]) {
                ap.length++;
            }
            if (ap.length > al) {
                if (ap.length == ap.edge->lengthTo(i) && ap.edge->link) {
                    ap.length = 0;
                    ap.node = ap.edge->link;
                }
                break;
            }
            if (ap.length > 0) {
                // Split edge in the middle
                auto newNode = new Node{};
                newNode->suffixLink = root;
                auto e1 = newNode->getEdgeInsert(s[i]);
                e1->first = i;
                e1->last = sLen;
                auto ac = ap.getStrPos();
                auto e2 = newNode->getEdgeInsert(s[ac]);
                *e2 = *ap.edge;
                e2->first = ac;
                ap.edge->link = newNode;
                ap.edge->last = ac;
                if (prevSplit) {
                    prevSplit->suffixLink = newNode;
                }
                prevSplit = newNode;
            } else {
                // Make a leaf edge
                DCHECK(ap.edge->empty());
                ap.edge->first = i;
                ap.edge->last = sLen;
            }
            // print(root, s, i + 1, [](auto const &s) { VLOG(1) << s; });
            remainingFirst++;
            if (ap.node != root) {
                ap.node = ap.node->suffixLink;
                ap.edge =
                        const_cast<Edge *>(ap.node->getEdge(s[ap.edge->first]));
            } else if (ap.length > 0) {
                DCHECK_LT(remainingFirst, sLen);
                DCHECK_LT(i, sLen);
                ap.length--;
                ap.edge = ap.node->getEdgeInsert(s[remainingFirst]);
            }
        }
    }
    DCHECK_EQ(remainingFirst, sLen);
    return root;
}

FindResult
findSubstr(Node const *root, char const *s, char const *p, uint32_t pLen) {
    ActivePoint ap{const_cast<Node *>(root)};
    for (uint32_t i = 0; i < pLen;) {
        if (ap.length == 0) {
            if (!ap.node) {
                return NotFound;
            }
            ap.edge = const_cast<Edge *>(ap.node->getEdge(p[i]));
            if (!ap.edge) {
                return NotFound;
            }
            ap.length++;
            i++;
            continue;
        }
        if (ap.length == ap.edge->length()) {
            ap.node = ap.edge->link;
            ap.length = 0;
            continue;
        }
        if (p[i] != s[ap.getStrPos()]) {
            return NotFound;
        }
        ap.length++;
        i++;
    }
    return {true, ap.getStrPos() - pLen};
}

void destroy(Node *node) {
    if (node != nullptr) {
        for (auto kv : node->e) {
            destroy(kv.second.link);
        }
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
    for (auto kv : node->e) {
        auto e = kv.second;
        auto trueLast = std::min(e.last, sLen);
        std::snprintf(
                buf,
                sizeof(buf),
                "Edge '%c' [%i, %i] %s link %p",
                kv.first,
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

using suffix_tree::Edge;
using suffix_tree::FindResult;
using suffix_tree::Node;
using suffix_tree::NotFound;

FindResult foundAt(uint32_t i) {
    return FindResult{true, i};
}

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
            for (auto kv : node->e) {
                buildRec(kv.second.link);
            }
        }
    }

    uint32_t id;
    std::unordered_map<Node const *, uint32_t> idMap;
};

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
    EXPECT_EQ(NAME, e->name(STR));

#define CHECK_LEAF_EDGE(NODE, STR, NAME)                                       \
    do {                                                                       \
        GET_EDGE(NODE, STR, NAME);                                             \
        EXPECT_TRUE(e->link == nullptr);                                       \
        EXPECT_TRUE(e->link == nullptr);                                       \
    } while (false)

#define CHECK_INTERNAL_EDGE(NODE, STR, NAME)                                   \
    do {                                                                       \
        GET_EDGE(NODE, STR, NAME);                                             \
        EXPECT_TRUE(e->link != nullptr);                                       \
    } while (false)

TEST(Ukkonen, Case1) {
    char const str[] = "xyx$";
    auto root = build(str);
    IdMap map{root};
    EXPECT_EQ(2, map.size());
    {
        auto node = root;
        EXPECT_EQ(3, node->numEdges());
        EXPECT_EQ(1, map.getId(node));
        EXPECT_EQ(0, map.getId(node->suffixLink));

        CHECK_LEAF_EDGE(node, str, "$");
        CHECK_INTERNAL_EDGE(node, str, "x");
        CHECK_LEAF_EDGE(node, str, "yx$");
    }
    {
        auto node = root->getEdge('x')->link;
        EXPECT_EQ(2, node->numEdges());
        EXPECT_EQ(2, map.getId(node));
        EXPECT_EQ(1, map.getId(node->suffixLink));

        CHECK_LEAF_EDGE(node, str, "$");
        CHECK_LEAF_EDGE(node, str, "yx$");
    }
    EXPECT_EQ(foundAt(0), findSubstr(root, str, "xyx", 3));
    EXPECT_EQ(foundAt(1), findSubstr(root, str, "yx", 2));
    EXPECT_EQ(foundAt(0), findSubstr(root, str, "x", 1));
    EXPECT_EQ(NotFound, findSubstr(root, str, "xx", 2));
    EXPECT_EQ(NotFound, findSubstr(root, str, "yz", 2));
    suffix_tree::destroy(root);
}

TEST(Ukkonen, Case2) {
    char const str[] = "xaxbxc$";
    auto root = build(str);
    IdMap map{root};
    EXPECT_EQ(2, map.size());
    {
        auto node = root;
        EXPECT_EQ(5, node->numEdges());
        EXPECT_EQ(1, map.getId(node));
        EXPECT_EQ(0, map.getId(node->suffixLink));

        CHECK_LEAF_EDGE(node, str, "$");
        CHECK_LEAF_EDGE(node, str, "axbxc$");
        CHECK_LEAF_EDGE(node, str, "bxc$");
        CHECK_LEAF_EDGE(node, str, "c$");
        CHECK_INTERNAL_EDGE(node, str, "x");
    }
    {
        auto node = root->getEdge('x')->link;
        EXPECT_EQ(3, node->numEdges());
        EXPECT_EQ(2, map.getId(node));
        EXPECT_EQ(1, map.getId(node->suffixLink));

        CHECK_LEAF_EDGE(node, str, "axbxc$");
        CHECK_LEAF_EDGE(node, str, "bxc$");
        CHECK_LEAF_EDGE(node, str, "c$");
    }
    EXPECT_EQ(foundAt(0), findSubstr(root, str, "xaxbxc", 6));
    EXPECT_EQ(foundAt(2), findSubstr(root, str, "xbx", 3));
    EXPECT_EQ(foundAt(4), findSubstr(root, str, "xc", 2));
    EXPECT_EQ(NotFound, findSubstr(root, str, "xbc", 3));
    EXPECT_EQ(NotFound, findSubstr(root, str, "xaxbxcx", 7));
    suffix_tree::destroy(root);
}

TEST(Ukkonen, Case3a) {
    char const str[] = "xyzxya$";
    auto root = build(str);
    IdMap map{root};
    EXPECT_EQ(3, map.size());
    {
        auto node = root;
        EXPECT_EQ(5, node->numEdges());
        EXPECT_EQ(1, map.getId(node));
        EXPECT_EQ(0, map.getId(node->suffixLink));

        CHECK_LEAF_EDGE(node, str, "$");
        CHECK_LEAF_EDGE(node, str, "a$");
        CHECK_INTERNAL_EDGE(node, str, "xy");
        CHECK_INTERNAL_EDGE(node, str, "y");
        CHECK_LEAF_EDGE(node, str, "zxya$");
    }
    {
        auto node = root->getEdge('x')->link;
        EXPECT_EQ(2, node->numEdges());
        EXPECT_EQ(2, map.getId(node));
        EXPECT_EQ(3, map.getId(node->suffixLink));

        CHECK_LEAF_EDGE(node, str, "a$");
        CHECK_LEAF_EDGE(node, str, "zxya$");
    }
    {
        auto node = root->getEdge('y')->link;
        EXPECT_EQ(2, node->numEdges());
        EXPECT_EQ(3, map.getId(node));
        EXPECT_EQ(1, map.getId(node->suffixLink));

        CHECK_LEAF_EDGE(node, str, "a$");
        CHECK_LEAF_EDGE(node, str, "zxya$");
    }
    EXPECT_EQ(foundAt(0), findSubstr(root, str, "xyz", 3));
    EXPECT_EQ(foundAt(3), findSubstr(root, str, "xya", 3));
    EXPECT_EQ(NotFound, findSubstr(root, str, "zxa", 3));
    EXPECT_EQ(NotFound, findSubstr(root, str, "yxa", 3));
    suffix_tree::destroy(root);
}

TEST(Ukkonen, Case3b) {
    char const str[] = "xyzxyaxyz$";
    auto root = build(str);
    IdMap map{root};
    EXPECT_EQ(6, map.size());
    {
        auto node = root;
        EXPECT_EQ(5, node->numEdges());
        EXPECT_EQ(1, map.getId(node));
        EXPECT_EQ(0, map.getId(node->suffixLink));

        CHECK_LEAF_EDGE(node, str, "$");
        CHECK_LEAF_EDGE(node, str, "axyz$");
        CHECK_INTERNAL_EDGE(node, str, "xy");
        CHECK_INTERNAL_EDGE(node, str, "y");
        CHECK_INTERNAL_EDGE(node, str, "z");
    }
    {
        auto node = root->getEdge('x')->link;
        EXPECT_EQ(2, node->numEdges());
        EXPECT_EQ(2, map.getId(node));
        EXPECT_EQ(4, map.getId(node->suffixLink));

        CHECK_LEAF_EDGE(node, str, "axyz$");
        CHECK_INTERNAL_EDGE(node, str, "z");
    }
    {
        auto node = root->getEdge('x')->link->getEdge('z')->link;
        EXPECT_EQ(2, node->numEdges());
        EXPECT_EQ(3, map.getId(node));
        EXPECT_EQ(5, map.getId(node->suffixLink));

        CHECK_LEAF_EDGE(node, str, "$");
        CHECK_LEAF_EDGE(node, str, "xyaxyz$");
    }
    {
        auto node = root->getEdge('y')->link;
        EXPECT_EQ(2, node->numEdges());
        EXPECT_EQ(4, map.getId(node));
        EXPECT_EQ(1, map.getId(node->suffixLink));

        CHECK_LEAF_EDGE(node, str, "axyz$");
        CHECK_INTERNAL_EDGE(node, str, "z");
    }
    {
        auto node = root->getEdge('y')->link->getEdge('z')->link;
        EXPECT_EQ(2, node->numEdges());
        EXPECT_EQ(5, map.getId(node));
        EXPECT_EQ(6, map.getId(node->suffixLink));

        CHECK_LEAF_EDGE(node, str, "$");
        CHECK_LEAF_EDGE(node, str, "xyaxyz$");
    }
    {
        auto node = root->getEdge('z')->link;
        EXPECT_EQ(2, node->numEdges());
        EXPECT_EQ(6, map.getId(node));
        EXPECT_EQ(1, map.getId(node->suffixLink));

        CHECK_LEAF_EDGE(node, str, "$");
        CHECK_LEAF_EDGE(node, str, "xyaxyz$");
    }
    EXPECT_EQ(foundAt(0), findSubstr(root, str, "xyz", 3));
    EXPECT_EQ(foundAt(3), findSubstr(root, str, "xyaxyz", 6));
    EXPECT_EQ(NotFound, findSubstr(root, str, "xyzz", 4));
    EXPECT_EQ(NotFound, findSubstr(root, str, "axzz", 4));
    suffix_tree::destroy(root);
}

TEST(Ukkonen, Case4) {
    char const str[] = "xxxx$";
    auto root = build(str);
    IdMap map{root};
    EXPECT_EQ(4, map.size());
    {
        auto node = root;
        EXPECT_EQ(2, node->numEdges());
        EXPECT_EQ(1, map.getId(node));
        EXPECT_EQ(0, map.getId(node->suffixLink));

        CHECK_LEAF_EDGE(node, str, "$");
        CHECK_INTERNAL_EDGE(node, str, "x");
    }
    {
        auto node = root->getEdge('x')->link;
        EXPECT_EQ(2, node->numEdges());
        EXPECT_EQ(2, map.getId(node));
        EXPECT_EQ(1, map.getId(node->suffixLink));

        CHECK_LEAF_EDGE(node, str, "$");
        CHECK_INTERNAL_EDGE(node, str, "x");
    }
    {
        auto node = root->getEdge('x')->link->getEdge('x')->link;
        EXPECT_EQ(2, node->numEdges());
        EXPECT_EQ(3, map.getId(node));
        EXPECT_EQ(2, map.getId(node->suffixLink));

        CHECK_LEAF_EDGE(node, str, "$");
        CHECK_INTERNAL_EDGE(node, str, "x");
    }
    {
        auto node = root->getEdge('x')
                            ->link->getEdge('x')
                            ->link->getEdge('x')
                            ->link;
        EXPECT_EQ(2, node->numEdges());
        EXPECT_EQ(4, map.getId(node));
        EXPECT_EQ(3, map.getId(node->suffixLink));

        CHECK_LEAF_EDGE(node, str, "$");
        CHECK_LEAF_EDGE(node, str, "x$");
    }
    EXPECT_EQ(foundAt(0), findSubstr(root, str, "xxxx", 4));
    EXPECT_EQ(foundAt(0), findSubstr(root, str, "xxx", 3));
    EXPECT_EQ(foundAt(0), findSubstr(root, str, "xx", 2));
    EXPECT_EQ(foundAt(0), findSubstr(root, str, "x", 1));
    EXPECT_EQ(NotFound, findSubstr(root, str, "xxxxx", 5));
    suffix_tree::destroy(root);
}

TEST(Ukkonen, Case5) {
    char const str[] = "abxabyabz$";
    auto root = build(str);
    IdMap map{root};
    EXPECT_EQ(3, map.size());
    {
        auto node = root;
        EXPECT_EQ(6, node->numEdges());
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
        auto node = root->getEdge('a')->link;
        EXPECT_EQ(3, node->numEdges());
        EXPECT_EQ(2, map.getId(node));
        EXPECT_EQ(3, map.getId(node->suffixLink));

        CHECK_LEAF_EDGE(node, str, "xabyabz$");
        CHECK_LEAF_EDGE(node, str, "yabz$");
        CHECK_LEAF_EDGE(node, str, "z$");
    }
    {
        auto node = root->getEdge('b')->link;
        EXPECT_EQ(3, node->numEdges());
        EXPECT_EQ(3, map.getId(node));
        EXPECT_EQ(1, map.getId(node->suffixLink));

        CHECK_LEAF_EDGE(node, str, "xabyabz$");
        CHECK_LEAF_EDGE(node, str, "yabz$");
        CHECK_LEAF_EDGE(node, str, "z$");
    }
    EXPECT_EQ(foundAt(0), findSubstr(root, str, "ab", 2));
    EXPECT_EQ(foundAt(2), findSubstr(root, str, "xaby", 4));
    EXPECT_EQ(foundAt(4), findSubstr(root, str, "bya", 3));
    EXPECT_EQ(foundAt(8), findSubstr(root, str, "z", 1));
    suffix_tree::destroy(root);
}

int main(int argc, char **argv) {
    initLog(argc, argv);
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
