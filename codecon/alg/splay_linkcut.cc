#include "tree.h"

void test1() {
    auto n10 = make_node(10);
    auto n30 = make_node(30);
    auto n40 = make_node(40);
    auto n50 = make_node(50);
    auto n60 = make_node(60);
    auto n80 = make_node(80);
    auto n90 = make_node(90);
    link(n50, link(n30, n10, n40), link(n80, n60, n90));
    auto sub = splay_split(n50);
    CHECK(sub.first == n50 && sub.second == n80);
    CHECK(n50->root());
    CHECK(n80->root());
    CHECK(walk(n50) == vector<int>({50, 30, 10, 0, 0, 40, 0, 0, 0}));
    CHECK(walk(n80) == vector<int>({80, 60, 0, 0, 90, 0, 0}));
    auto linked = splay_join(sub.first, sub.second);
    CHECK(walk(linked) ==
          vector<int>({50, 30, 10, 0, 0, 40, 0, 0, 80, 60, 0, 0, 90, 0, 0}));
    delete_tree(linked);
}

void test2() {
    auto n10 = make_node(10);
    auto n30 = make_node(30);
    auto n40 = make_node(40);
    auto n50 = make_node(50);
    auto n60 = make_node(60);
    auto n80 = make_node(80);
    auto n90 = make_node(90);
    link(n50, link(n30, n10, n40), link(n80, n60, n90));
    auto sub = splay_split(n40);
    CHECK(sub.first == n40 && sub.second == n50);
    CHECK(n40->root());
    CHECK(n50->root());
    CHECK(walk(n40) == vector<int>({40, 30, 10, 0, 0, 0, 0}));
    CHECK(walk(n50) == vector<int>({50, 0, 80, 60, 0, 0, 90, 0, 0}));
    auto linked = splay_join(sub.first, sub.second);
    CHECK(walk(linked) ==
          vector<int>({40, 30, 10, 0, 0, 0, 50, 0, 80, 60, 0, 0, 90, 0, 0}));
    delete_tree(linked);
}

void test3() {
    auto n10 = make_node(10);
    auto n30 = make_node(30);
    auto n40 = make_node(40);
    auto n50 = make_node(50);
    auto n60 = make_node(60);
    auto n80 = make_node(80);
    auto n90 = make_node(90);
    link(n50, link(n30, n10, n40), link(n80, n60, n90));
    auto sub = splay_split(n80);
    CHECK(sub.first == n80 && sub.second == n90);
    CHECK(n80->root());
    CHECK(n90->root());
    CHECK(walk(n80) ==
          vector<int>({80, 50, 30, 10, 0, 0, 40, 0, 0, 60, 0, 0, 0}));
    CHECK(walk(n90) == vector<int>({90, 0, 0}));
    // Move 60 to root.
    splay(n60);
    CHECK(walk(n60) ==
          vector<int>({60, 50, 30, 10, 0, 0, 40, 0, 0, 0, 80, 0, 0}));
    auto linked = splay_join(sub.first, sub.second);
    CHECK(walk(linked) ==
          vector<int>({80, 60, 50, 30, 10, 0, 0, 40, 0, 0, 0, 0, 90, 0, 0}));
    delete_tree(linked);
}

int main(int argc, char **argv) {
    ios_base::sync_with_stdio(false);
    test1();
    test2();
    test3();
    cout << "TESTS PASSED." << endl;
    return 0;
}
