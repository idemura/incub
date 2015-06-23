#include "base.h"

template<class T>
struct Node {
  T data;
  Node *p = nullptr, *l = nullptr, *r = nullptr;

  Node(): data() {}
  explicit Node(T data): data(data) {}
  Node(T data, Node *l, Node* r): data(data), l(l), r(r) {
    if (l != nullptr) l->p = this;
    if (r != nullptr) r->p = this;
  }
};

template<class T>
void delete_tree(Node<T> *node) {
  if (node == nullptr) return;
  vector<Node<T>*> q{node};
  for (int i = 0; i < q.size(); i++) {
    auto n = q[i];
    if (n->l) q.push_back(n->l);
    if (n->r) q.push_back(n->r);
    delete n;
  }
}

template<class T>
void rotate(Node<T> *node) {
  auto up = node->p;
  if (up == nullptr) return;
  node->p = up->p;
  if (up->p) {
    if (up->p->l == up)
      up->p->l = node;
    else
      up->p->r = node;
  }
  up->p = node;
  if (up->l == node) {
    up->l = node->r;
    if (up->l) up->l->p = up;
    node->r = up;
  } else {
    up->r = node->l;
    if (up->r) up->r->p = up;
    node->l = up;
  }
}

template<class T>
Node<T>* find_root(Node<T> *node) {
  if (node == nullptr) return nullptr;
  while (node->p) {
    node = node->p;
  }
  return node;
}

using NodeInt = Node<int>;

void test1() {
  auto t = new NodeInt(50);
  rotate(t);
  t = find_root(t);
  CHECK(t->data == 50);
  CHECK(t->p == nullptr);
  CHECK(t->l == nullptr);
  CHECK(t->r == nullptr);
  delete_tree(t);
}

/**
  B    A
 /  =>  \
A        B
**/
void test2() {
  auto t = new NodeInt(50, new NodeInt(30), nullptr);
  rotate(t->l);
  t = find_root(t);
  CHECK(t->data == 30);
  CHECK(t->p == nullptr);
  CHECK(t->l == nullptr);
  CHECK(t->r->data == 50);
  CHECK(t->r->p == t);
  CHECK(t->r->l == nullptr);
  CHECK(t->r->r == nullptr);
  delete_tree(t);
}

/**
B        A
 \  =>  /
  A    B
**/
void test3() {
  auto t = new NodeInt(30, nullptr, new NodeInt(50));
  rotate(t->r);
  t = find_root(t);
  CHECK(t->data == 50);
  CHECK(t->p == nullptr);
  CHECK(t->l->data == 30);
  CHECK(t->l->p == t);
  CHECK(t->l->l == nullptr);
  CHECK(t->l->r == nullptr);
  CHECK(t->r == nullptr);
  delete_tree(t);
}

/**
    50        25
   / \       / \
  25  75 => 20  50
 / \           / \
20  30        30  75
**/
void test4() {
  auto t = new NodeInt(50,
      new NodeInt(25, new NodeInt(20), new NodeInt(30)),
      new NodeInt(75));
  rotate(t->l);
  t = find_root(t);
  CHECK(t->data == 25);
  CHECK(t->p == nullptr);
  CHECK(t->l->data == 20);
  CHECK(t->l->p == t);
  CHECK(t->l->l == nullptr);
  CHECK(t->l->r == nullptr);
  CHECK(t->r->data == 50);
  CHECK(t->r->p == t);
  CHECK(t->r->l->data == 30);
  CHECK(t->r->l->p == t->r);
  CHECK(t->r->l->l == nullptr);
  CHECK(t->r->l->r == nullptr);
  CHECK(t->r->r->data == 75);
  CHECK(t->r->r->p == t->r);
  CHECK(t->r->r->l == nullptr);
  CHECK(t->r->r->r == nullptr);
  // Now rotate back to the initial state.
  rotate(t->r);
  t = find_root(t);
  CHECK(t->data == 50);
  CHECK(t->p == nullptr);
  CHECK(t->l->data == 25);
  CHECK(t->l->p == t);
  CHECK(t->l->l->data == 20);
  CHECK(t->l->l->p == t->l);
  CHECK(t->l->l->l == nullptr);
  CHECK(t->l->l->r == nullptr);
  CHECK(t->l->r->data == 30);
  CHECK(t->l->r->p == t->l);
  CHECK(t->l->r->l == nullptr);
  CHECK(t->l->r->r == nullptr);
  CHECK(t->r->data == 75);
  CHECK(t->r->p == t);
  CHECK(t->r->l == nullptr);
  CHECK(t->r->r == nullptr);
  delete_tree(t);
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
  auto t = new NodeInt(40,
      nullptr,
      new NodeInt(50,
          new NodeInt(25, new NodeInt(20), nullptr),
          nullptr));
  rotate(t->r->l);
  t = find_root(t);
  CHECK(t->data == 40);
  CHECK(t->p == nullptr);
  CHECK(t->l == nullptr);
  CHECK(t->r->data == 25);
  CHECK(t->r->p == t);
  CHECK(t->r->l->data == 20);
  CHECK(t->r->l->p == t->r);
  CHECK(t->r->l->l == nullptr);
  CHECK(t->r->l->r == nullptr);
  CHECK(t->r->r->data == 50);
  CHECK(t->r->r->p == t->r);
  CHECK(t->r->r->l == nullptr);
  CHECK(t->r->r->r == nullptr);
  delete_tree(t);
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
  auto t = new NodeInt(40,
      nullptr,
      new NodeInt(50,
          nullptr,
          new NodeInt(75, nullptr, new NodeInt(80))));
  rotate(t->r->r);
  t = find_root(t);
  CHECK(t->data == 40);
  CHECK(t->p == nullptr);
  CHECK(t->l == nullptr);
  CHECK(t->r->data == 75);
  CHECK(t->r->p == t);
  CHECK(t->r->l->data == 50);
  CHECK(t->r->l->p == t->r);
  CHECK(t->r->l->l == nullptr);
  CHECK(t->r->l->r == nullptr);
  CHECK(t->r->r->data == 80);
  CHECK(t->r->r->p == t->r);
  CHECK(t->r->r->l == nullptr);
  CHECK(t->r->r->r == nullptr);
  delete_tree(t);
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
