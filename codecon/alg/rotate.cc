#include "tree.h"

constexpr Node<int> *kNULL = nullptr;

void test1() {
    auto n50 = make_node(50);
    rotate(n50);
    CHECK(n50->root());
    CHECK(n50->leaf());
    delete_tree(n50);
}

/**
  50    30
 /   =>  \
30        50
**/
void test2() {
    auto n30 = make_node(30);
    auto n50 = make_node(50);
    link(n50, n30, kNULL);
    rotate(n30);
    CHECK(n30->root());
    CHECK(is_l(n30, kNULL));
    CHECK(is_r(n30, n50));
    CHECK(n50->leaf());
    delete_tree(n30);
}

/**
30        50
 \   =>  /
  50    30
**/
void test3() {
    auto n30 = make_node(30);
    auto n50 = make_node(50);
    link(n30, kNULL, n50);
    rotate(n50);
    CHECK(n50->root());
    CHECK(is_l(n50, n30));
    CHECK(is_r(n50, kNULL));
    CHECK(n30->leaf());
    delete_tree(n30);
}

/**
    50        25
   / \       / \
  25  75 => 20  50
 / \           / \
20  30        30  75
**/
void test4() {
    auto n20 = make_node(20);
    auto n25 = make_node(25);
    auto n30 = make_node(30);
    auto n50 = make_node(50);
    auto n75 = make_node(75);
    link(n50, link(n25, n20, n30), n75);
    rotate(n25);
    CHECK(n25->root());
    CHECK(is_l(n25, n20));
    CHECK(n20->leaf());
    CHECK(is_r(n25, n50));
    CHECK(is_l(n50, n30));
    CHECK(n30->leaf());
    CHECK(is_r(n50, n75));
    CHECK(n75->leaf());
    // Now rotate back to the initial state.
    rotate(n50);
    CHECK(n50->root());
    CHECK(is_l(n50, n25));
    CHECK(is_l(n25, n20));
    CHECK(n20->leaf());
    CHECK(is_r(n25, n30));
    CHECK(n30->leaf());
    CHECK(is_r(n50, n75));
    CHECK(n75->leaf());
    delete_tree(n50);
}

/**
  40       40
   \        \
    50 =>    25
   /        / \
  25       20  50
 /
20
**/
void test5() {
    auto n20 = make_node(20);
    auto n25 = make_node(25);
    auto n40 = make_node(40);
    auto n50 = make_node(50);
    link(n40, kNULL, link(n50, link(n25, n20, kNULL), kNULL));
    rotate(n25);
    CHECK(n40->root());
    CHECK(is_l(n40, kNULL));
    CHECK(is_r(n40, n25));
    CHECK(is_l(n25, n20));
    CHECK(n20->leaf());
    CHECK(is_r(n25, n50));
    CHECK(n50->leaf());
    delete_tree(n40);
}

/**
40           40
 \            \
  50     =>    75
   \          / \
    75       50  80
     \
      80
**/
void test6() {
    auto n40 = make_node(40);
    auto n50 = make_node(50);
    auto n75 = make_node(75);
    auto n80 = make_node(80);
    link(n40, kNULL, link(n50, kNULL, link(n75, kNULL, n80)));
    rotate(n75);
    CHECK(n40->root());
    CHECK(is_l(n40, kNULL));
    CHECK(is_r(n40, n75));
    CHECK(is_l(n75, n50));
    CHECK(n50->leaf());
    CHECK(is_r(n75, n80));
    CHECK(n80->leaf());
    delete_tree(n40);
}

int main(int argc, char **argv) {
    ios_base::sync_with_stdio(false);
    test1();
    test2();
    test3();
    test4();
    test5();
    test6();
    cout << "TESTS PASSED." << endl;
    return 0;
}
