#include <cstdint>
#include <functional>
#include <random>
#include <utility>
#include <vector>

#include "log.h"
#include <gtest/gtest.h>

template <class K, class V, class KCmp = std::less<K>>
class RandMeldHeap: private KCmp {
public:
    explicit RandMeldHeap() = default;

    // Melds two heaps
    RandMeldHeap(RandMeldHeap &&h1, RandMeldHeap &&h2):
            root_{meld(h1.root_, h2.root_)},
            size_{h1.size_ + h2.size_} {
        h1.reset();
        h2.reset();
    }

    RandMeldHeap(RandMeldHeap const &) = delete;
    RandMeldHeap &operator=(RandMeldHeap const &) = delete;

    RandMeldHeap(RandMeldHeap &&other): root_{other.root_}, size_{other.size_} {
        other.reset();
    }

    RandMeldHeap &operator=(RandMeldHeap &&other) {
        this->~RandMeldHeap();
        return *new (this) RandMeldHeap{other};
    }

    ~RandMeldHeap() {
        destroy(root_);
        reset();
    }

    void push(K k, V v) {
        root_ = meld(root_, new Node{std::move(k), std::move(v)});
        size_++;
    }

    const std::pair<K, V> &top() const {
        return root_->kv;
    }

    void pop() {
        auto r = root_;
        root_ = meld(r->br[0], r->br[1]);
        delete r;
        size_--;
    }

    bool empty() const {
        return root_ == nullptr;
    }

    size_t size() const {
        return size_;
    }

private:
    struct Node {
        std::pair<K, V> kv;
        Node *br[2]{};

        Node(K k, V v): kv{std::move(k), std::move(v)} {}
    };

    Node *meld(Node *n1, Node *n2) {
        if (n1 == nullptr) return n2;
        if (n2 == nullptr) return n1;
        if ((*this)(n2->kv.first, n1->kv.first)) {
            std::swap(n1, n2);
        }
        auto k = rg() & 1;
        n1->br[k] = meld(n1->br[k], n2);
        return n1;
    }

    void reset() {
        root_ = nullptr;
        size_ = 0;
    }

    static void destroy(Node *n) {
        if (n) {
            destroy(n->br[0]);
            destroy(n->br[1]);
            delete n;
        }
    }

    static std::minstd_rand0 rg;
    Node *root_{nullptr};
    size_t size_{0};
};
template <class K, class V, class KCmp>
std::minstd_rand0 RandMeldHeap<K, V, KCmp>::rg{0};

TEST(Heap, PushPop) {
    RandMeldHeap<int, std::string> h;
    h.push(2, "world");
    h.push(1, "hello");
    h.push(3, "cat");
    h.push(5, "dog");
    h.push(4, "fox");
    EXPECT_EQ(5, h.size());
    EXPECT_FALSE(h.empty());
    EXPECT_EQ(1, h.top().first);
    EXPECT_EQ("hello", h.top().second);
    h.pop();
    EXPECT_FALSE(h.empty());
    EXPECT_EQ(2, h.top().first);
    EXPECT_EQ("world", h.top().second);
    h.pop();
    EXPECT_EQ(3, h.top().first);
    EXPECT_EQ("cat", h.top().second);
    h.pop();
    EXPECT_EQ(4, h.top().first);
    EXPECT_EQ("fox", h.top().second);
    h.pop();
    EXPECT_EQ(5, h.top().first);
    EXPECT_EQ("dog", h.top().second);
    h.pop();
    EXPECT_TRUE(h.empty());
}

TEST(Heap, Meld) {
    RandMeldHeap<int, std::string> h1, h2;
    h1.push(2, "world");
    h1.push(3, "cat");
    EXPECT_EQ(2, h1.size());

    h2.push(5, "dog");
    h2.push(4, "fox");
    h2.push(1, "hello");
    EXPECT_EQ(3, h2.size());

    RandMeldHeap<int, std::string> h{std::move(h1), std::move(h2)};
    EXPECT_EQ(5, h.size());
    EXPECT_FALSE(h.empty());
    EXPECT_EQ(1, h.top().first);
    EXPECT_EQ("hello", h.top().second);
    h.pop();
    EXPECT_FALSE(h.empty());
    EXPECT_EQ(2, h.top().first);
    EXPECT_EQ("world", h.top().second);
    h.pop();
    EXPECT_EQ(3, h.top().first);
    EXPECT_EQ("cat", h.top().second);
    h.pop();
    EXPECT_EQ(4, h.top().first);
    EXPECT_EQ("fox", h.top().second);
    h.pop();
    EXPECT_EQ(5, h.top().first);
    EXPECT_EQ("dog", h.top().second);
    h.pop();
    EXPECT_TRUE(h.empty());
}

int main(int argc, char **argv) {
    initLog(argc, argv);
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
