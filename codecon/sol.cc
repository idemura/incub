#include <algorithm>
#include <map>
#include <set>
#include <string>
#include <vector>
#include <utility>
#include <assert.h>
#include <ctype.h>
#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

#define ARRAY_SIZEOF(A) (sizeof(A) / sizeof(A[0]))
#define INF 0x7fffffff
#define NON_COPYABLE(C) \
    C(const C&); \
    C& operator=(const C&);

typedef long long int lli;

class Treap {
  NON_COPYABLE(Treap);
public:
  explicit Treap(int p): p_(p), root_() {}
  ~Treap() { clear(); }
  int upper(int k);
  void insert(int n);
  void remove(int n);
  void clear();
  void printKeys();
  void print();
  void check();
  void expand();

private:
  struct Node {
    Node *l, *r;
    int prio;
    int n;
    int d;

    Node(): l(), r(), prio(rand()), n(), d() {}
    int getKey(int p)
    {
      if (d != 0) {
        n = (n + d) % p;
        if (l) {
          l->update(d, p);
        }
        if (r) {
          r->update(d, p);
        }
        d = 0;
      }
      return n;
    }

    void update(int d1, int p)
    {
      d = (d + d1) % p;
    }
  };

  static void clearRec(Node *node);
  Node* upperRec(Node *node, int k);
  Node* merge(Node *lst, Node *rst);
  // All key `lst` < k, and `k <= rst`.
  void split(Node *node, int k, Node **lst, Node **rst);
  void printKeysRec(Node *node);
  void printRec(Node *node, int depth);
  void checkTreeRec(Node *node, int min, int max);
  void checkHeapRec(Node *node, int p);
  void expandRec(Node *node);

  int p_;
  Node *root_;
};

void Treap::clear()
{
  clearRec(root_);
  root_ = nullptr;
}

void Treap::clearRec(Node *node)
{
  if (!node) return;
  clearRec(node->l);
  clearRec(node->r);
  delete node;
}

int Treap::upper(int k)
{
  auto where = upperRec(root_, k);
  if (!where) {
    return -1;  // Something less than `k`.
  }
  return where->n;
}

Treap::Node* Treap::upperRec(Node *node, int k)
{
  if (!node) return nullptr;
  int node_k = node->getKey(p_);
  if (node_k == k) {
    return node;
  }
  if (k < node_k) {
    return upperRec(node->l, k);
  } else {
    return upperRec(node->r, k);
  }
}

Treap::Node* Treap::merge(Node *lst, Node *rst)
{
  if (!lst) return rst;
  if (!rst) return lst;
  // We know all keys of `n1` are less than each key of `n2`.
  if (lst->prio < rst->prio) {
    lst->getKey(p_);  // Actualize the key.
    lst->r = merge(lst->r, rst);
    return lst;
  } else {
    rst->getKey(p_);  // Actualize the key.
    rst->l = merge(lst, rst->l);
    return rst;
  }
}

void Treap::split(Node *node, int k, Node **lst, Node **rst)
{
  if (!node) {
    *lst = *rst = nullptr;
    return;
  }
  if (k <= node->getKey(p_)) {
    split(node->l, k, lst, rst);
    node->l = *rst;
    *rst = node;
  } else {
    split(node->r, k, lst, rst);
    node->r = *lst;
    *lst = node;
  }
}

void Treap::insert(int n)
{
  printf("insert %d\n", n);
  Node *lst = nullptr, *rst = nullptr, *new_node = new Node;
  split(root_, n, &lst, &rst);
  new_node->n = n;
  root_ = merge(merge(lst, new_node), rst);
}

void Treap::remove(int n)
{
  Node *lst = nullptr, *rst = nullptr, *n_node = nullptr;
  printf("remove %d\n", n);
  split(root_, n, &lst, &rst);
  printKeysRec(lst);
  printf("\n");
  printKeysRec(rst);
  printf("\n");
  printf("--------\n");
  split(rst, n + 1, &n_node, &rst);
  printKeysRec(n_node);
  printf("\n");
  printKeysRec(rst);
  printf("\n");
  // Before merge, we add modifiers:
  printf("--------\n");
  printKeysRec(lst);
  printf("\n");
  printKeysRec(rst);
  printf("\n");
  if (n_node) {
    delete n_node;
    if (lst) {
      lst->d = -n + p_;
    }
    if (rst) {
      rst->d = -n;
    }
  }
  printf("++++\n");
  printRec(rst, 0);
  printf("++++\n");
  printRec(lst, 0);
  // Now all numbers on the right are less than on the left, so why the order.
  root_ = merge(rst, lst);
  printf("---- merged: ----\n");
  printRec(root_, 0);
  printf("----\n");
}

void Treap::printKeys()
{
  printKeysRec(root_);
  printf("\n");
}

void Treap::printKeysRec(Node *node)
{
  if (!node) return;
  printKeysRec(node->l);
  // printf("%d ", node->getKey(p_));
  if (node->d) {
    printf("%d(d %d) ", node->n, node->d);
  } else {
    printf("%d ", node->n);
  }
  printKeysRec(node->r);
}

void Treap::print()
{
  printRec(root_, 0);
}

void Treap::printRec(Node *node, int depth)
{
  if (!node) return;
  for (int i = 0; i < depth; i++) {
    printf("  ");
  }
  printf("%p n %d d %d prio %d\n", node, node->n, node->d, node->prio);
  printRec(node->l, depth + 1);
  printRec(node->r, depth + 1);
}

void Treap::check()
{
  checkTreeRec(root_, INT_MIN, INT_MAX);
  checkHeapRec(root_, INT_MIN);
}

void Treap::checkTreeRec(Node *node, int min, int max)
{
  if (!node) return;
  // Our treap stores only different keys.
  if (!(min < node->getKey(p_) && node->getKey(p_) < max)) {
    printf("TREE CHECK FAILED: %d is not in %d - %d\n", node->n, min, max);
    assert(false);
  }
  checkTreeRec(node->l, min, std::min(max, node->n));
  checkTreeRec(node->r, std::max(min, node->n), max);
}

void Treap::checkHeapRec(Node *node, int p)
{
  if (!node) return;
  if (!(node->prio >= p)) {
    printf("HEAP CHECK FAILED: %d greater %d\n", node->prio, p);
    assert(false);
  }
  checkHeapRec(node->l, node->prio);
  checkHeapRec(node->r, node->prio);
}

void Treap::expand()
{
  expandRec(root_);
}

void Treap::expandRec(Node *node)
{
  if (!node) return;
  node->getKey(p_);
  expandRec(node->l);
  expandRec(node->r);
}


int getMinModuloP(std::vector<int> &v, int k, int p)
{
  Treap t(p);
  int s = 0;
  for (int i = 0; i < v.size(); i++) {
    s = (s + v[i]) % p;
    t.insert(s);
  }
  s = 0;
  int upper_min = INT_MAX;
  for (int i = 0; i < v.size(); i++) {
    s = (s + v[i]) % p;
    int upper_k = t.upper(k);
    if (upper_k == k) {
      return k;
    }
    if (upper_k < upper_min) upper_min = upper_k;
    t.remove(s);
  }
  return upper_min;
}

int main(int argc, char **argv)
{
#ifndef ONLINE_JUDGE
  freopen("in", "r", stdin);
#endif
  int n = 0, k = 0, p = 0;
  scanf("%d%d%d", &n, &k, &p);
  printf("k %d p %d\n", k, p);
  auto v = std::vector<int>(n);
  for (int i = 0; i < n; i++) {
    scanf("%d", &v[i]);
    printf("%d ", v[i]);
  }
  printf("\n");
  printf("All possible:\n");
  for (size_t i = 0; i < v.size(); i++) {
    printf("%2zu: ", i);
    int s = 0;
    for (int j = i; j < v.size(); j++) {
      s = (s + v[j]) % p;
      printf("%2d ", s);
    }
    printf("\n");
  }
  // printf("%d\n", getMinModuloP(v, k, p));

  Treap t(13);
  t.print();
  t.insert(0);
  t.print();
  t.insert(1);
  t.print();
  t.insert(2);
  t.print();
  t.insert(3);
  t.print();
  t.insert(4);
  t.print();
  t.insert(5);
  t.print();
  t.insert(6);
  t.print();
  t.insert(7);
  t.print();
  t.check();
  t.printKeys();

  printf("------------\n");
  t.remove(3);
  t.print();
  t.printKeys();
  t.expand();
  t.printKeys();
  return 0;
}
