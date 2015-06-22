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
  auto p = node->p;
  if (p == nullptr) return;
  node->p = p->p;
  if (p->p) {
    if (p->p->l == p) {
      p->p->l = node;
    } else {
      p->p->r = node;
    }
  }
  if (p->l == node) {
    p->l = node->r;
    node->r = p;
  } else {
    p->r = node->l;
    node->l = p;
  }
  p->p = node;
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

void check_root(NodeInt *n, int v) {
  CHECK(n->p == nullptr);
  CHECK(n->data == v);
}

void check_l(NodeInt *n, int v) {
  CHECK(n->l != nullptr);
  CHECK(n->l->p == n);
  CHECK(n->l->data == v);
}
void check_l(NodeInt *n) {
  CHECK(n->l == nullptr);
}

void check_r(NodeInt *n, int v) {
  CHECK(n->r != nullptr);
  CHECK(n->r->p == n);
  CHECK(n->r->data == v);
}
void check_r(NodeInt *n) {
  CHECK(n->r == nullptr);
}

void test1() {
  auto t = new NodeInt(50);
  rotate(t);
  t = find_root(t);
  check_root(t, 50);
  check_l(t);
  check_r(t);
  delete_tree(t);
}

void test2() {
  auto t = new NodeInt(50, new NodeInt(30), nullptr);
  rotate(t->l);
  t = find_root(t);
  check_root(t, 30);
  check_l(t);
  check_r(t, 50);
  check_l(t->r);
  check_r(t->r);
  delete_tree(t);
}

void test3() {
  auto t = new NodeInt(30, nullptr, new NodeInt(50));
  rotate(t->r);
  t = find_root(t);
  check_root(t, 50);
  check_l(t, 30);
  check_l(t->l);
  check_r(t->l);
  check_r(t);
  delete_tree(t);
}

int main(int argc, char **argv) {
  ios_base::sync_with_stdio(false);
  test1();
  test2();
  test3();
  cout << "TESTS PASSED." << endl;
  return 0;
}
