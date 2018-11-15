#include "ukkonen.h"

#include <algorithm>
#include <cstdio>
#include <cstring>
#include <functional>
#include <iostream>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#include "log.h"
#include <gtest/gtest.h>

namespace suffix_tree {
namespace {
std::string substr(char const *s, int first, int last) {
    return std::string(s + first, last - first);
}
} // namespace

int toIndex(int c) {
    if (c == EoLn) {
        return 0;
    }
    int i = c - Base + 1;
    DCHECK(1 <= i && i <= AlphabetSize) << "i " << i;
    return i;
}

std::string Edge::name(char const *s) const {
    DCHECK(valid());
    return substr(s, first, last);
}

Edge Node::getEdge(char c) const {
    return e[toIndex(c)];
}

int Node::countEdges() const {
    int n = 0;
    for (int i = 0; i <= AlphabetSize; i++) {
        if (e[i].valid()) {
            n++;
        }
    }
    return n;
}

// Ukkonen algorithm to build a suffix tree
Node *build(char const *s, int sLen) {
    auto root = new Node{};
    if (sLen == 0) {
        return root;
    }
    // root->suffixLink = root;
    DCHECK_EQ(EoLn, s[sLen - 1]);
    VLOG(1) << "root " << root;
    int remainingFirst = 0;
    auto activeNode = root;
    int activeEdge = -1;
    int activeLength = 0;
    for (int i = 0; i < sLen; i++) {
        VLOG(1) << "---------- NEW CHAR";
        print(root, s, i, [](std::string const &s) { VLOG(1) << s; });
        VLOG(1) << "----------";
        int e = toIndex(s[i]);
        VLOG(1) << "i " << i << " char " << s[i] << " index " << e;
        // remaining++;
        Node *prevSplit = nullptr;
        while (i >= remainingFirst) {
            // If there is no Edge to move in the Node, create a leaf Edge.
            VLOG(1) << "remainingFirst " << remainingFirst << " activeNode "
                    << activeNode << " activeEdge " << activeEdge
                    << " activeLength " << activeLength;
            // Check if we need to move next node
            auto al = activeLength;
            if (activeLength == 0) {
                // Try to start a path in this node
                VLOG(1) << "try start a path in node";
                if (activeNode->e[e].valid() &&
                        s[activeNode->e[e].first] == s[i]) {
                    activeEdge = e;
                    activeLength++;
                    DCHECK_EQ(1, activeLength);
                    VLOG(1) << "path started";
                }
            } else {
                // Try increase current path
                VLOG(1) << "try increase path along the edge";
                int activeChar = activeNode->e[activeEdge].first + activeLength;
                if (s[activeChar] == s[i]) {
                    activeLength++;
                }
            }
            if (activeLength > al) {
                VLOG(1) << "path advanced, break while";
                if (activeLength == activeNode->e[activeEdge].lengthTo(i)) {
                    VLOG(1) << "advance to next node";
                    activeLength = 0;
                    activeNode = activeNode->e[activeEdge].link;
                    DCHECK_NOTNULL(activeNode);
                }
                break;
            }

            VLOG(1) << "create an edge or split";
            if (activeLength > 0) {
                // Split edge in the middle
                VLOG(1) << "split edge in the middle activeEdge " << activeEdge;
                // DCHECK_GE(activeChar, 0);
                auto newNode = new Node{};
                VLOG(1) << "new node " << newNode;
                VLOG(1) << "new edge " << s[i];
                newNode->suffixLink = root;
                newNode->e[e].first = i;
                newNode->e[e].last = sLen;
                int activeChar = activeNode->e[activeEdge].first + activeLength;
                VLOG(1) << "active char " << activeChar << " or "
                        << s[activeChar];
                int k = toIndex(s[activeChar]);
                newNode->e[k].first = activeChar;
                newNode->e[k].last = activeNode->e[activeEdge].last;
                newNode->e[k].link = activeNode->e[activeEdge].link;
                activeNode->e[activeEdge].link = newNode;
                activeNode->e[activeEdge].last = activeChar;
                VLOG(1) << "new node with edge "
                        << activeNode->e[activeEdge].name(s);
                if (prevSplit) {
                    prevSplit->suffixLink = newNode;
                }
                prevSplit = newNode;
            } else {
                // Make a leaf edge
                VLOG(1) << "make a leaf edge " << s[i];
                DCHECK(!activeNode->e[e].valid())
                        << "should be not initialized";
                activeNode->e[e].first = i;
                activeNode->e[e].last = sLen;
            }
            remainingFirst++;
            if (activeNode == root) {
                if (remainingFirst < sLen) {
                    activeEdge = toIndex(s[remainingFirst]);
                    VLOG(1) << "activate new edge in root: " << activeEdge;
                }
                if (activeLength > 0) activeLength--;
            } else {
                activeNode = activeNode->suffixLink;
            }
            VLOG(1) << "--- tree after split";
            print(root, s, i + 1, [](std::string const &s) { VLOG(1) << s; });
        }
        VLOG(1) << "while loop done, remainingFirst " << remainingFirst;
    }
    DCHECK_EQ(remainingFirst, sLen);
    VLOG(1) << "build done";
    // root->suffixLink = nullptr;
    VLOG(1) << "RESULT";
    print(root, s, sLen, [](std::string const &s) { VLOG(1) << s; });
    return root;
}

void destroy(Node *root) {
    if (root == nullptr) {
        return;
    }
    for (int i = 0; i < AlphabetSize; i++) {
        destroy(root->e[i].link);
    }
}

namespace {
void printRec(Node const *node,
        char const *s,
        int sLen,
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
    for (int i = 0; i <= AlphabetSize; i++) {
        auto e = node->e[i];
        if (!e.valid()) {
            continue;
        }
        auto c = i == 0 ? EoLn : char(Base + i - 1);
        auto trueLast = std::min(e.last, sLen);
        std::snprintf(buf,
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
} // namespace

void print(Node const *root, char const *s, int len, PrintFn const &fn) {
    printRec(root, s, len, "", fn);
}
} // namespace suffix_tree

using suffix_tree::AlphabetSize;
using suffix_tree::Edge;
using suffix_tree::Node;

namespace {
class IdMap {
public:
    explicit IdMap(Node const *root): id{1} {
        buildRec(root);
    }

    int getId(Node const *node) const {
        auto iter = idMap.find(node);
        return iter == idMap.end() ? 0 : iter->second;
    }

    size_t size() const {
        return idMap.size();
    }

private:
    void buildRec(Node const *node) {
        if (node != nullptr) {
            idMap[node] = id++;
            for (int i = 0; i <= AlphabetSize; i++) {
                buildRec(node->e[i].link);
            }
        }
    }

    int id;
    std::unordered_map<Node const *, int> idMap;
};

int validEdgeCount(Node const *node) {
    int res = 0;
    for (int i = 0; i <= AlphabetSize; i++) {
        if (node->e[i].valid()) {
            res++;
        }
    }
    return res;
}

suffix_tree::Node *build(char const *s) {
    VLOG(1) << s;
    std::cout << "Input " << s << std::endl;
    auto sLen = (int)std::strlen(s);
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

void test1() {
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

void test2() {
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

void test3() {
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
    suffix_tree::destroy(root);
}
} // namespace

int main(int argc, char **argv) {
    initLog(argc, argv);
    test1();
    test2();
    test3();
    return 0;
}
