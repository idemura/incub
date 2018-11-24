#include <cassert>
#include <chrono>
#include <cstdint>
#include <functional>
#include <iostream>
#include <map>
#include <random>
#include <string>
#include <utility>
#include <vector>

#include <glog/logging.h>
#include <gtest/gtest.h>

using i32 = int32_t;
using i64 = int64_t;

template <typename K, typename V>
class aa_tree {
public:
    aa_tree() = default;
    aa_tree(aa_tree const &) = delete;
    aa_tree(aa_tree &&other) noexcept: root_{other.root_}, size_{other.size_} {
        other.reset();
    }
    ~aa_tree() noexcept {
        destroy_rec(root_);
        reset();
    }

    aa_tree &operator=(aa_tree const &) = delete;
    aa_tree &operator=(aa_tree &&other) noexcept {
        this->~aa_tree();
        return *new (this) aa_tree(std::move(other));
    }

    void insert(K k, V v) {
        root_ = insert_rec(k, std::move(v), root_);
    }

    void remove(K k) {
        root_ = remove_rec(k, root_);
    }

    V *lookup(K k) const {
        for (auto n = root_; n != nil; n = n->child[k > n->key]) {
            if (n->key == k) {
                return &n->val;
            }
        }
        return nullptr;
    }

    size_t size() const {
        return size_;
    }

    void print(std::ostream &os) {
        print_rec(os, root_, 0);
    }

private:
    struct node;

    struct link {
        node *child[2]{(node *)&s_sentinel, (node *)&s_sentinel};
        uint32_t height{0};
    };

    struct node: link {
        K key{};
        V val{};

        node(K k, V v): key{k}, val{std::move(v)} {}
    };

    static link s_sentinel;
    static node *nil;
    node *root_{nil};
    size_t size_{};

    void reset() {
        root_ = nil;
        size_ = 0;
    }

    static void destroy_rec(node *n) {
        if (n != nil) {
            destroy_rec(n->child[0]);
            destroy_rec(n->child[1]);
            delete n;
        }
    }

    node *insert_rec(K k, V v, node *n) {
        if (n == nil) {
            size_++;
            auto new_node = new node{k, std::move(v)};
            new_node->height = 1;
            return new_node;
        }
        if (n->key == k) {
            n->val = v;
            return n;
        }
        unsigned ci = k > n->key;
        n->child[ci] = insert_rec(k, std::move(v), n->child[ci]);
        return split(skew(n));
    }

    node *remove_rec(K k, node *n) {
        if (n == nil) {
            return n; // Not found
        }
        if (n->key == k) {
            if (n->child[0] == nil || n->child[1] == nil) {
                size_--;
                auto t = n;
                n = n->child[n->child[0] == nil];
                delete t;
            } else {
                auto next = n->child[0];
                while (next->child[1] != nil) {
                    next = next->child[1];
                }
                n->key = next->key;
                n->val = std::move(next->val);
                n->child[0] = remove_rec(next->key, n->child[0]);
            }
        } else {
            unsigned ci = k > n->key;
            n->child[ci] = remove_rec(k, n->child[ci]);
        }
        if (n->height - 1 > n->child[0]->height ||
            n->height - 1 > n->child[1]->height) {
            n->height--;
            n = split(skew(n));
        }
        return n;
    }

    static node *skew(node *n) {
        if (n->height == n->child[0]->height) {
            auto t = n->child[0];
            n->child[0] = t->child[1];
            t->child[1] = n;
            return t;
        } else {
            return n;
        }
    }

    static node *split(node *n) {
        if (n->height == n->child[1]->height &&
            n->height == n->child[1]->child[1]->height) {
            auto t = n->child[1];
            n->child[1] = t->child[0];
            t->child[0] = n;
            t->height++;
            return t;
        } else {
            return n;
        }
    }

    static void print_rec(std::ostream &os, node *n, uint32_t tab) {
        os << std::string(tab * 2, ' ') << conv(n) << " ";
        if (n == nil) {
            os << "h=" << n->height << "\n";
        } else {
            os << "h=" << n->height << " key=" << n->key << " val=" << n->val
               << " c[0]=" << conv(n->child[0]) << " c[1]=" << conv(n->child[1])
               << "\n";
            print_rec(os, n->child[0], tab + 1);
            print_rec(os, n->child[1], tab + 1);
        }
    }

    static node *conv(node *n) {
        return n == nil ? nullptr : n;
    }
};

template <typename K, typename V>
typename aa_tree<K, V>::link aa_tree<K, V>::s_sentinel;

template <typename K, typename V>
typename aa_tree<K, V>::node *aa_tree<K, V>::nil = (node *)&s_sentinel;

aa_tree<i32, i32> make_simple_tree() {
    aa_tree<i32, i32> t;
    t.insert(500, 1);
    t.insert(200, 2);
    t.insert(100, 3);
    t.insert(600, 4);
    t.insert(400, 5);
    t.insert(150, 6);
    return t;
}

void test_insert() {
    auto t = make_simple_tree();
    EXPECT_EQ(6, t.size());
    EXPECT_EQ(1, *t.lookup(500));
    EXPECT_EQ(2, *t.lookup(200));
    EXPECT_EQ(3, *t.lookup(100));
    EXPECT_EQ(4, *t.lookup(600));
    EXPECT_EQ(5, *t.lookup(400));
    EXPECT_EQ(6, *t.lookup(150));
}

void test_remove_one_child() {
    auto t = make_simple_tree();
    t.remove(100);
    EXPECT_EQ(5, t.size());
    EXPECT_EQ(1, *t.lookup(500));
    EXPECT_EQ(2, *t.lookup(200));
    EXPECT_EQ(4, *t.lookup(600));
    EXPECT_EQ(5, *t.lookup(400));
    EXPECT_EQ(6, *t.lookup(150));
}

void test_remove_two_children1() {
    auto t = make_simple_tree();
    t.remove(500);
    EXPECT_EQ(5, t.size());
    EXPECT_EQ(2, *t.lookup(200));
    EXPECT_EQ(3, *t.lookup(100));
    EXPECT_EQ(4, *t.lookup(600));
    EXPECT_EQ(5, *t.lookup(400));
    EXPECT_EQ(6, *t.lookup(150));
}

void test_remove_two_children2() {
    auto t = make_simple_tree();
    t.remove(200);
    EXPECT_EQ(5, t.size());
    EXPECT_EQ(1, *t.lookup(500));
    EXPECT_EQ(3, *t.lookup(100));
    EXPECT_EQ(4, *t.lookup(600));
    EXPECT_EQ(5, *t.lookup(400));
    EXPECT_EQ(6, *t.lookup(150));
}

void test() {
    std::cout << "Unit test\n";
    test_insert();
    test_remove_one_child();
    test_remove_two_children1();
    test_remove_two_children2();
}

template <typename K, typename V>
class std_map {
public:
    void insert(K k, V v) {
        map[k] = v;
    }

    void remove(K k) {
        map.erase(k);
    }

    V *lookup(K k) const {
        auto itr = map.find(k);
        if (itr == map.end()) {
            return nullptr;
        } else {
            return const_cast<V *>(&itr->second);
        }
    }

private:
    std::map<K, V> map;
};

void test_rand(size_t num_inserts, size_t num_removes) {
    std::cout << "Random ops test\n";
    num_removes = std::min(num_inserts, num_removes);
    aa_tree<i32, i32> t;
    std::vector<i32> keys;
    std::minstd_rand0 rg{1};
    for (size_t i = 0; i < num_inserts; i++) {
        auto k = (i32)rg();
        t.insert(k, k / 16);
        keys.push_back(k);
    }
    for (size_t i = 0; i < num_removes; i++) {
        t.remove(keys[i]);
    }
    for (size_t i = num_removes; i < num_inserts; i++) {
        EXPECT_EQ(keys[i] / 16, *t.lookup(keys[i]));
    }
}

template <typename Map>
void perf_map(size_t num_ops, std::function<i32()> gen) {
    Map m;
    std::vector<i32> keys(num_ops);
    auto start = std::chrono::system_clock::now();
    for (size_t i = 0; i < num_ops; i++) {
        keys[i] = gen();
        m.insert(keys[i], keys[i] / 16);
    }
    for (size_t i = 0; i < num_ops / 2; i++) {
        m.remove(keys[i]);
    }
    auto dur = std::chrono::duration_cast<std::chrono::milliseconds>(
            std::chrono::system_clock::now() - start);
    std::cout << dur.count() << " ms\n";
    std::cerr << "check " << m.lookup(1) << "\n";
}

std::function<i32()> getGen(bool random) {
    if (random) {
        return [rg = std::minstd_rand0{1}]() mutable { return rg(); };
    } else {
        return [n = 0]() mutable { return n++; };
    }
}

int main(int argc, char **argv) {
    gflags::ParseCommandLineFlags(&argc, &argv, true /* remove_flags */);
    google::InitGoogleLogging(argv[0]);
    test();
    test_rand(1'000, 400);
    // std::cout << "Perf test\n";
    // const size_t num_inserts{100'000};
    // auto gen = getGen(true);
    // std::cout << num_inserts << "inserts\n";
    // for (int i = 0; i < 10; i++) {
    //     std::cout << "std::map\t";
    //     perf_map<std_map<i32, i32>>(num_inserts, gen);
    //     std::cout << "aa_tree \t";
    //     perf_map<aa_tree<i32, i32>>(num_inserts, gen);
    // }
    return 0;
}
