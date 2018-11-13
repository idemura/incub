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

#include <glog/logging.h>
#include <gtest/gtest.h>

#define dlog LOG(ERROR)

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

// Ukkonen algorithm to build a suffix tree
Node *build(char const *s, int sLen) {
    auto root = new Node{};
    if (sLen == 0) {
        return root;
    }
    DCHECK_EQ(EoLn, s[sLen - 1]);
    dlog << "root " << root;
    int remaining = 0;
    auto activeNode = root;
    int activeEdge = -1;
    int activeLength = 0;
    for (int i = 0; i < sLen; i++) {
        dlog << "";
        print(root, s, i, [](std::string const &s) { dlog << s; });
        dlog << "";
        dlog << "i " << i << " char is " << s[i];
        int c = toIndex(s[i]);
        remaining++;
        Node *prevSplit = nullptr;
        while (remaining > 0) {
            dlog << "--- while loop, remaining " << remaining;
            // If there is no Edge to move in the Node, create a leaf Edge.
            dlog << "activeNode " << activeNode;
            dlog << "activeEdge " << activeEdge;
            dlog << "activeLength " << activeLength;
            // Check if we need to move next node
            auto al = activeLength;
            if (activeLength == 0) {
                // Try to start a path in this node
                dlog << "try start a path from Node";
                if (activeNode->e[c].valid() &&
                    s[activeNode->e[c].first] == s[i]) {
                    activeEdge = c;
                    activeLength++;
                    DCHECK_EQ(1, activeLength);
                    dlog << "path started";
                }
            } else {
                // Try increase current path
                dlog << "try increase path along the edge";
                int activeChar = activeNode->e[activeEdge].first + activeLength;
                if (s[activeChar] == s[i]) {
                    activeLength++;
                }
            }
            if (activeLength > al) {
                dlog << "path advanced, break while";
                if (activeLength == activeNode->e[activeEdge].lengthTo(i)) {
                    dlog << "advance to next node";
                    activeLength = 0;
                    activeNode = activeNode->e[activeEdge].link;
                    DCHECK(activeNode != nullptr);
                }
                break;
            }
            dlog << "create an edge or split";
            if (activeLength == 0) {
                // Make a leaf edge
                dlog << "make a leaf edge";
                DCHECK_LT(activeNode->e[c].first, 0)
                        << "should be not initialized";
                activeNode->e[c].first = i;
                activeNode->e[c].last = sLen;
            } else {
                // Split edge in the middle
                dlog << "split edge in the middle";
                // DCHECK_GE(activeChar, 0);
                auto newNode = new Node{};
                dlog << "new node " << newNode;
                newNode->suffixLink = root;
                newNode->e[c].first = i;
                newNode->e[c].last = sLen;
                int activeChar = activeNode->e[activeEdge].first + activeLength;
                dlog << "active char " << activeChar << " or " << s[activeChar];
                int k = toIndex(s[activeChar]);
                newNode->e[k].first = activeChar;
                newNode->e[k].last = activeNode->e[activeEdge].last;
                newNode->e[k].link = activeNode->e[activeEdge].link;
                activeNode->e[activeEdge].link = newNode;
                activeNode->e[activeEdge].last = activeChar;
                if (prevSplit) {
                    prevSplit->suffixLink = newNode;
                }
                prevSplit = newNode;
                activeLength--;
                dlog << "activeEdge before: " << activeEdge;
                activeEdge = toIndex(s[activeChar - 1]);
                dlog << "activeEdge now: " << activeEdge;
            }
            activeNode = activeNode->suffixLink;
            if (activeNode == nullptr) {
                dlog << "loop into root";
                activeNode = root;
            }
            remaining--;
        }
        dlog << "while loop done, remaining " << remaining;
    }
    dlog << "build done";
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
void printRec(
        Node const *node,
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
        std::snprintf(
                buf,
                sizeof(buf),
                "Node '%c' [%i, %i] %s link %p",
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
    explicit IdMap(Node const *root): id_{1} {
        buildRec(root);
    }

    int getId(Node const *node) const {
        auto iter = idMap_.find(node);
        return iter == idMap_.end() ? 0 : iter->second;
    }

    size_t size() const {
        return idMap_.size();
    }

private:
    void buildRec(Node const *node) {
        if (node != nullptr) {
            idMap_[node] = id_++;
            for (int i = 0; i <= AlphabetSize; i++) {
                buildRec(node->e[i].link);
            }
        }
    }

    int id_;
    std::unordered_map<Node const *, int> idMap_;
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
    dlog << s;
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
        EXPECT_EQ(1, map.getId(node));
        EXPECT_EQ(0, map.getId(node->suffixLink));

        CHECK_LEAF_EDGE(node, str, "$");
        CHECK_INTERNAL_EDGE(node, str, "x");
        CHECK_LEAF_EDGE(node, str, "yx$");
    }
    {
        auto node = root->getEdge('x').link;
        EXPECT_EQ(2, map.getId(node));
        EXPECT_EQ(1, map.getId(node->suffixLink));

        CHECK_LEAF_EDGE(node, str, "$");
        CHECK_LEAF_EDGE(node, str, "yx$");
    }
    suffix_tree::destroy(root);
}
} // namespace

int main(int argc, char **argv) {
    google::InitGoogleLogging(argv[0]);
    test1();
    // testString("xaxbxc$");
    // testString("xyzxya$");
    // testString("xyzxyaxyz$");
    return 0;
}
