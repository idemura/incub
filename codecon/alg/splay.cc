#include "tree.h"

void test1() {
    auto n50 = make_node(50);
    splay(n50);
    CHECK(n50->root());
    CHECK(n50->leaf());
    delete_tree(n50);
}

/**
    50        25
   / \       / \
  25  60 => 20  50
 / \           / \
20  30        30  60
**/
void test2() {
    auto n20 = make_node(20);
    auto n25 = make_node(25);
    auto n30 = make_node(30);
    auto n50 = make_node(50);
    auto n60 = make_node(60);
    link(n50, link(n25, n20, n30), n60);
    splay(n25);
    CHECK(n25->root());
    CHECK(is_l(n25, n20));
    CHECK(n20->leaf());
    CHECK(is_r(n25, n50));
    CHECK(is_l(n50, n30));
    CHECK(n30->leaf());
    CHECK(is_r(n50, n60));
    CHECK(n60->leaf());
    delete_tree(n25);
}

// zig-zig.
/**
      50        20
     / \       / \
    30  55 => 15  30
   / \           / \
  20  35        25  50
 / \               / \
15  25            35  55
**/
void test3() {
    auto n15 = make_node(15);
    auto n20 = make_node(20);
    auto n25 = make_node(25);
    auto n30 = make_node(30);
    auto n35 = make_node(35);
    auto n50 = make_node(50);
    auto n55 = make_node(55);
    link(n50, link(n30, link(n20, n15, n25), n35), n55);
    splay(n20);
    CHECK(n20->root());
    CHECK(is_l(n20, n15));
    CHECK(n15->leaf());
    CHECK(is_r(n20, n30));
    CHECK(is_l(n30, n25));
    CHECK(n25->leaf());
    CHECK(is_r(n30, n50));
    CHECK(is_l(n50, n35));
    CHECK(n35->leaf());
    CHECK(is_r(n50, n55));
    CHECK(n55->leaf());
    delete_tree(n20);
}

/**
  50                80
 / \               / \
45  70     =>     70  85
   / \           / \
  65  80        50  75
     / \       / \
    75  85    45  65
**/
void test4() {
    auto n45 = make_node(45);
    auto n50 = make_node(50);
    auto n65 = make_node(65);
    auto n70 = make_node(70);
    auto n75 = make_node(75);
    auto n80 = make_node(80);
    auto n85 = make_node(85);
    link(n50, n45, link(n70, n65, link(n80, n75, n85)));
    splay(n80);
    CHECK(n80->root());
    CHECK(is_l(n80, n70));
    CHECK(is_l(n70, n50));
    CHECK(is_l(n50, n45));
    CHECK(n45->leaf());
    CHECK(is_r(n50, n65));
    CHECK(n65->leaf());
    CHECK(is_r(n70, n75));
    CHECK(n75->leaf());
    CHECK(is_r(n80, n85));
    CHECK(n85->leaf());
    delete_tree(n80);
}

// zig-zag.
/**
    50           40
   / \          /   \
  30  55 =>   30     50
 / \         / \    / \
25  40      25  35 45  55
   / \
  35  45
**/
void test5() {
    auto n25 = make_node(25);
    auto n30 = make_node(30);
    auto n35 = make_node(35);
    auto n40 = make_node(40);
    auto n45 = make_node(45);
    auto n50 = make_node(50);
    auto n55 = make_node(55);
    link(n50, link(n30, n25, link(n40, n35, n45)), n55);
    splay(n40);
    CHECK(n40->root());
    CHECK(is_l(n40, n30));
    CHECK(is_l(n30, n25));
    CHECK(n25->leaf());
    CHECK(is_r(n30, n35));
    CHECK(n35->leaf());
    CHECK(is_r(n40, n50));
    CHECK(is_l(n50, n45));
    CHECK(n45->leaf());
    CHECK(is_r(n50, n55));
    CHECK(n55->leaf());
    delete_tree(n40);
}

/**
  50               60
 / \              /   \
45  70     =>   50     70
   / \         / \    / \
  60  75      45  55 65  75
 / \
55  65
**/
void test6() {
    auto n45 = make_node(45);
    auto n50 = make_node(50);
    auto n55 = make_node(55);
    auto n60 = make_node(60);
    auto n65 = make_node(65);
    auto n70 = make_node(70);
    auto n75 = make_node(75);
    link(n50, n45, link(n70, link(n60, n55, n65), n75));
    splay(n60);
    CHECK(n60->root());
    CHECK(is_l(n60, n50));
    CHECK(is_l(n50, n45));
    CHECK(n45->leaf());
    CHECK(is_r(n50, n55));
    CHECK(n55->leaf());
    CHECK(is_r(n60, n70));
    CHECK(is_l(n70, n65));
    CHECK(n65->leaf());
    CHECK(is_r(n70, n75));
    CHECK(n75->leaf());
    delete_tree(n60);
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
