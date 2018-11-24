#include "base.h"

// We are leaking memory. Sets should probably GC them or kill all
// dijoint sets at once.
struct Node {
    Node* parent = nullptr;
    int rank = 1;
};

class NodePool {
public:
    NodePool() = default;
    ~NodePool() {
        for (auto n : nodes_)
            delete n;
    }

    Node* make_set() {
        auto n = new Node();
        nodes_.push_back(n);
        return n;
    }

private:
    vector<Node*> nodes_;
    NON_COPYABLE(NodePool);
};

Node* get_root(Node* a) {
    auto r = a;
    while (r->parent != nullptr) {
        r = r->parent;
    }
    // Compress path.
    auto p = a;
    while (p != r) {
        auto t = p->parent;
        p->parent = r;
        p = t;
    }
    return r;
}

// Returns node of the result set.
Node* set_union(Node* a, Node* b) {
    a = get_root(a);
    b = get_root(b);
    if (a == b) return a;
    if (a->rank == b->rank) {
        b->parent = a;
        a->rank++;
        return a;
    } else {
        if (a->rank < b->rank) {
            a->parent = b;
            return b;
        } else {
            b->parent = a;
            return a;
        }
    }
}

void test() {
    NodePool pool;
    auto s1 = pool.make_set();
    auto s2 = pool.make_set();
    CHECK(get_root(s1) != get_root(s2));
    auto s3 = set_union(s1, s2);
    CHECK(s3 == s1);
    CHECK(s3->rank == 2);
    CHECK(s2->rank == 1);
    CHECK(get_root(s2) == s1);
    auto s4 = pool.make_set();
    set_union(s2, s4);
    CHECK(get_root(s4) == s1);
    CHECK(get_root(s2) == s1);
}

int main(int argc, char** argv) {
    ios_base::sync_with_stdio(false);
    test();
    cout << "TESTS PASSED." << endl;
    return 0;
}
