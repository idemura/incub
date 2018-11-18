#include <algorithm>
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <utility>

constexpr uint32_t AlphabetSize = 26 * 2;
constexpr char EoLn = '$';

static uint32_t toIndex(char c) {
    if (c == EoLn) {
        return 0;
    }
    static_assert('A' < 'a', "char order");
    int i = (c - 'A' <= 26 ? c - 'A' : c - 'a' + 26);
    return (uint32_t)i + 1;
}

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

    Edge getEdge(char c) const {
        return e[toIndex(c)];
    }
};

struct ActivePoint {
    explicit ActivePoint(Node *node): node{node} {}

    uint32_t getActiveChar() const {
        return node->e[edge].first + length;
    }

    Node *node{};
    uint32_t length{};
    uint32_t edge{}; // Undefined if @length == 0
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
        // print(root, s, i, vlogStr);
        auto e = toIndex(s[i]);
        Node *prevSplit = nullptr;
        while (i >= remainingFirst) {
            // Try to move along the edge (or start a path if length == 0).
            auto al = ap.length;
            if (ap.length == 0) {
                if (ap.node->e[e].valid()) {
                    ap.edge = e;
                    ap.length++;
                }
            } else if (s[ap.getActiveChar()] == s[i]) {
                ap.length++;
            }
            if (ap.length > al) {
                if (ap.length == ap.node->e[ap.edge].lengthTo(i) &&
                    ap.node->e[ap.edge].link) {
                    ap.length = 0;
                    ap.node = ap.node->e[ap.edge].link;
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
                ap.node->e[e].first = i;
                ap.node->e[e].last = sLen;
            }
            remainingFirst++;
            if (ap.node != root) {
                ap.node = ap.node->suffixLink;
            } else if (ap.length > 0) {
                ap.length--;
                ap.edge = toIndex(s[remainingFirst]);
            }
        }
    }
    return root;
}

std::pair<bool, uint32_t>
findSubstr(Node const *root, char const *s, char const *p, uint32_t pLen) {
    ActivePoint ap{const_cast<Node *>(root)};
    for (uint32_t i = 0; i < pLen;) {
        auto e = toIndex(p[i]);
        if (ap.length == 0) {
            if (!ap.node || !ap.node->e[e].valid()) {
                return {false, 0};
            }
            ap.edge = e;
            ap.length++;
            i++;
            continue;
        }
        if (ap.length == ap.node->e[ap.edge].length()) {
            ap.node = ap.node->e[ap.edge].link;
            ap.length = 0;
            continue;
        }
        if (p[i] != s[ap.getActiveChar()]) {
            return {false, 0};
        }
        ap.length++;
        i++;
    }
    return {true, ap.getActiveChar() - pLen};
}

void destroy(Node *root) {
    if (root != nullptr) {
        for (uint32_t i = 0; i < AlphabetSize; i++) {
            destroy(root->e[i].link);
        }
    }
}

int main(int argc, char **argv) {
    static char s[100 * 1000 + 4];
    static char q[1000 + 4];
    int numCases = 0;
    scanf("%d", &numCases);
    while (numCases-- > 0) {
        int numQ = 0;
        scanf("%s%d", s, &numQ);
        auto sLen = (uint32_t)strlen(s);
        s[sLen] = '$';
        auto root = build(s, sLen + 1);
        while (numQ-- > 0) {
            scanf("%s", q);
            auto res = findSubstr(root, s, q, (uint32_t)strlen(q));
            printf("%s\n", (res.first ? "y" : "n"));
        }
        destroy(root);
    }
    return 0;
}
