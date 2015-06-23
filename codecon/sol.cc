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
Node<T>* find_root(Node<T> *node) {
  if (node == nullptr) return nullptr;
  while (node->p) {
    node = node->p;
  }
  return node;
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

// Splays node to the root.
template<class T>
void splay(Node<T> *node) {
  if (node == nullptr) return;
  while (node->p != nullptr) {
    if (node->p->p) {
      if ((node->p->l == node) == (node->p->p->l == node->p)) {
        // zig-zig
        rotate(node->p);
        rotate(node);
      } else {
        // zig-zag
        rotate(node);
        rotate(node);
      }
    } else {
      rotate(node);
    }
  }
}

using NodeInt = Node<int>;

void test1() {
  auto t = new NodeInt(50);
  splay(t);
  CHECK(t->data == 50);
  CHECK(t->p == nullptr);
  CHECK(t->l == nullptr);
  CHECK(t->r == nullptr);
  delete_tree(t);
}

/**
    50        25
   / \       / \
  25  60 => 20  50
 / \           / \
20  30        30  60
**/
void test2() {
  auto t = new NodeInt(50,
      new NodeInt(25, new NodeInt(20), new NodeInt(30)),
      new NodeInt(60));
  t = t->l;
  splay(t);
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
  CHECK(t->r->r->data == 60);
  CHECK(t->r->r->p == t->r);
  CHECK(t->r->r->l == nullptr);
  CHECK(t->r->r->r == nullptr);
  delete_tree(t);
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
  auto t = new NodeInt(50,
      new NodeInt(30,
          new NodeInt(20, new NodeInt(15), new NodeInt(25)),
          new NodeInt(35)),
      new NodeInt(55));
  t = t->l->l;
  splay(t);
  CHECK(t->data == 20);
  CHECK(t->p == nullptr);
  CHECK(t->l->data == 15);
  CHECK(t->l->p == t);
  CHECK(t->l->l == nullptr);
  CHECK(t->l->r == nullptr);
  CHECK(t->r->data == 30);
  CHECK(t->r->p == t);
  CHECK(t->r->l->data == 25);
  CHECK(t->r->l->p == t->r);
  CHECK(t->r->l->l == nullptr);
  CHECK(t->r->l->r == nullptr);
  CHECK(t->r->r->data == 50);
  CHECK(t->r->r->p == t->r);
  CHECK(t->r->r->l->data == 35);
  CHECK(t->r->r->l->p == t->r->r);
  CHECK(t->r->r->l->l == nullptr);
  CHECK(t->r->r->l->r == nullptr);
  CHECK(t->r->r->r->data == 55);
  CHECK(t->r->r->r->p == t->r->r);
  CHECK(t->r->r->r->l == nullptr);
  CHECK(t->r->r->r->r == nullptr);
  delete_tree(t);
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
  auto t = new NodeInt(50,
      new NodeInt(45),
      new NodeInt(70,
          new NodeInt(65),
          new NodeInt(80, new NodeInt(75), new NodeInt(85))));
  t = t->r->r;
  splay(t);
  CHECK(t->data == 80);
  CHECK(t->p == nullptr);
  CHECK(t->l->data == 70);
  CHECK(t->l->p == t);
  CHECK(t->l->l->data == 50);
  CHECK(t->l->l->p == t->l);
  CHECK(t->l->l->l->data == 45);
  CHECK(t->l->l->l->p == t->l->l);
  CHECK(t->l->l->l->l == nullptr);
  CHECK(t->l->l->l->r == nullptr);
  CHECK(t->l->l->r->data == 65);
  CHECK(t->l->l->r->p == t->l->l);
  CHECK(t->l->l->r->l == nullptr);
  CHECK(t->l->l->r->r == nullptr);
  CHECK(t->l->r->data == 75);
  CHECK(t->l->r->p == t->l);
  CHECK(t->l->r->l == nullptr);
  CHECK(t->l->r->r == nullptr);
  CHECK(t->r->data == 85);
  CHECK(t->r->p == t);
  CHECK(t->r->l == nullptr);
  CHECK(t->r->r == nullptr);
  delete_tree(t);
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
  auto t = new NodeInt(50,
      new NodeInt(30,
          new NodeInt(25),
          new NodeInt(40, new NodeInt(35), new NodeInt(45))),
      new NodeInt(55));
  t = t->l->r;
  splay(t);
  CHECK(t->data == 40);
  CHECK(t->p == nullptr);
  CHECK(t->l->data == 30);
  CHECK(t->l->p == t);
  CHECK(t->l->l->data == 25);
  CHECK(t->l->l->p == t->l);
  CHECK(t->l->l->l == nullptr);
  CHECK(t->l->l->r == nullptr);
  CHECK(t->l->r->data == 35);
  CHECK(t->l->r->p == t->l);
  CHECK(t->l->r->l == nullptr);
  CHECK(t->l->r->r == nullptr);
  CHECK(t->r->data == 50);
  CHECK(t->r->p == t);
  CHECK(t->r->l->data == 45);
  CHECK(t->r->l->p == t->r);
  CHECK(t->r->l->l == nullptr);
  CHECK(t->r->l->r == nullptr);
  CHECK(t->r->r->data == 55);
  CHECK(t->r->r->p == t->r);
  CHECK(t->r->r->l == nullptr);
  CHECK(t->r->r->r == nullptr);
  delete_tree(t);
}

/**
  50              60
 / \             /   \
45  70     =>   50     70
   / \         / \    / \
  60  75      45  55 65  75
 / \
55  65
**/
void test6() {
  auto t = new NodeInt(50,
      new NodeInt(45),
      new NodeInt(70,
          new NodeInt(60, new NodeInt(55), new NodeInt(65)),
          new NodeInt(75)));
  t = t->r->l;
  splay(t);
  CHECK(t->data == 60);
  CHECK(t->p == nullptr);
  CHECK(t->l->data == 50);
  CHECK(t->l->p == t);
  CHECK(t->l->l->data == 45);
  CHECK(t->l->l->p == t->l);
  CHECK(t->l->l->l == nullptr);
  CHECK(t->l->l->r == nullptr);
  CHECK(t->l->r->data == 55);
  CHECK(t->l->r->p == t->l);
  CHECK(t->l->r->l == nullptr);
  CHECK(t->l->r->r == nullptr);
  CHECK(t->r->data == 70);
  CHECK(t->r->p == t);
  CHECK(t->r->l->data == 65);
  CHECK(t->r->l->p == t->r);
  CHECK(t->r->l->l == nullptr);
  CHECK(t->r->l->r == nullptr);
  CHECK(t->r->r->data == 75);
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
