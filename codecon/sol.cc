#include <algorithm>
#include <map>
#include <set>
#include <string>
#include <vector>
#include <utility>
#include <assert.h>
#include <ctype.h>
#include <limits.h>
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

typedef std::pair<int, int> Key;

class Treap {
  NON_COPYABLE(Treap);
public:
  explicit Treap(int p): p_(p), root_() {}
  ~Treap() { clear(); }
  Key upper(Key key);
  void insert(Key key);
  void remove(Key key);
  void clear();
  void printKeys();
  void print();
  void check();
  void expand();

private:
  struct Node {
    Node *l, *r;
    int prio;
    Key key;
    int tree_key;

    Node(): l(), r(), prio(rand()), key(), tree_key() {}

    void update(int p)
    {
      if (tree_key != 0) {
        key.first = ((lli)key.first + tree_key) % p;
        if (l) {
          l->tree_key = ((lli)l->tree_key + tree_key) % p;
        }
        if (r) {
          r->tree_key = ((lli)r->tree_key + tree_key) % p;
        }
        tree_key = 0;
      }
    }
  };

  void print(Node *root);
  static void clearRec(Node *node);
  Node* upperRec(Node *node, Key key, Node *left);
  Node* merge(Node *lst, Node *rst);
  // All key `lst` < k, and `k <= rst`.
  void split(Node *node, Key key, Node **lst, Node **rst, bool right = true);
  void printKeysRec(Node *node);
  void printRec(Node *node, int depth, int idx);
  void checkTreeRec(Node *node, int min, int max);
  void checkHeapRec(Node *node, int prio);
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

Key Treap::upper(Key key)
{
  auto where = upperRec(root_, key, nullptr);
  if (!where) {
    return Key(-1, 0);  // Something less than `k`.
  }
  return where->key;
}

Treap::Node* Treap::upperRec(Node *node, Key key, Node *left)
{
  if (!node) return left;
  node->update(p_);
  if (node->key == key) {
    return node;
  }
  if (key < node->key) {
    return upperRec(node->l, key, node);
  } else {
    return upperRec(node->r, key, left);
  }
}

Treap::Node* Treap::merge(Node *lst, Node *rst)
{
  if (!lst) return rst;
  if (!rst) return lst;
  // We know all keys of `n1` are less than each key of `n2`.
  if (lst->prio < rst->prio) {
    lst->update(p_);
    lst->r = merge(lst->r, rst);
    return lst;
  } else {
    rst->update(p_);
    rst->l = merge(lst, rst->l);
    return rst;
  }
}

void Treap::split(Node *node, Key key, Node **lst, Node **rst, bool right)
{
  if (!node) {
    *lst = *rst = nullptr;
    return;
  }
  node->update(p_);
  if (key < node->key || (key == node->key && right)) {
    split(node->l, key, lst, &node->l, right);
    *rst = node;
  } else {
    split(node->r, key, &node->r, rst, right);
    *lst = node;
  }
}

void Treap::insert(Key key)
{
  Node *lst = nullptr, *rst = nullptr, *new_node = new Node;
  new_node->key = key;
  split(root_, new_node->key, &lst, &rst);
  root_ = merge(merge(lst, new_node), rst);
}

void Treap::remove(Key key)
{
  Node *lst = nullptr, *rst = nullptr, *node = nullptr;
  split(root_, key, &lst, &rst);
  split(rst, key, &node, &rst, false);
  if (node) {
    delete node;
    if (lst) {
      lst->tree_key = -key.first + p_;
    }
    if (rst) {
      rst->tree_key = -key.first;
    }
  }
  // Now all numbers on the right are less than on the left, so why the order.
  root_ = merge(rst, lst);
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
  // node->update(p_);
  if (node->tree_key) {
    printf("%d(tree_key %d) ", node->key.first, node->tree_key);
  } else {
    printf("%d ", node->key.first);
  }
  printKeysRec(node->r);
}

void Treap::print()
{
  print(root_);
}

void Treap::print(Node *root)
{
  printRec(root, 0, -1);
}

void Treap::printRec(Node *node, int depth, int idx)
{
  if (!node) return;
  for (int i = 0; i < depth; i++) {
    printf("  ");
  }
  if (idx >= 0) {
    printf("%c ", "lr"[idx]);
  }
  printf("%p key %d/%d tree_key %d prio %d\n", node, node->key.first,
         node->key.second, node->tree_key, node->prio);
  printRec(node->l, depth + 1, 0);
  printRec(node->r, depth + 1, 1);
}

void Treap::check()
{
  checkTreeRec(root_, INT_MIN, INT_MAX);
  checkHeapRec(root_, INT_MIN);
}

void Treap::checkTreeRec(Node *node, int min, int max)
{
  if (!node) return;
  node->update(p_);
  // Our treap stores only different keys.
  if (!(min < node->key.first && node->key.first < max)) {
    printf("TREE CHECK FAILED: %d is not in the range %d - %d\n",
           node->key.first, min, max);
    assert(false);
  }
  checkTreeRec(node->l, min, std::min(max, node->key.first));
  checkTreeRec(node->r, std::max(min, node->key.first), max);
}

void Treap::checkHeapRec(Node *node, int prio)
{
  if (!node) return;
  if (!(node->prio >= prio)) {
    printf("HEAP CHECK FAILED: %d greater %d\n", node->prio, prio);
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
  node->update(p_);
  expandRec(node->l);
  expandRec(node->r);
}


int getMinModuloP(const std::vector<int> &v, int k, int p)
{
  Treap t(p);
  int sum = 0;
  for (int i = 0; i < v.size(); i++) {
    sum = ((lli)sum + v[i]) % p;
    t.insert(Key(sum, i));
  }
  int min_gap = INT_MAX;
  for (int i = 0; i < v.size(); i++) {
    auto upper_key = t.upper(Key(k, 0));
    if (upper_key.first < 0) {
      // upper_key = t.upper(Key(0, 0));
      upper_key.first = p;
    }
    if (upper_key.first == k) {
      return k;
    }
    int gap = ((lli)upper_key.first - k + p) % p;
    if (gap < min_gap) {
      min_gap = gap;
    }
    t.remove(Key(v[i], i));
  }
  return (k + min_gap) % p;
}

int main(int argc, char **argv)
{
#ifndef ONLINE_JUDGE
  freopen("in", "r", stdin);
#endif
  int n = 0, k = 0, p = 0;
  scanf("%d%d%d", &n, &k, &p);
  auto v = std::vector<int>(n);
  for (int i = 0; i < v.size(); i++) {
    scanf("%d", &v[i]);
  }
  printf("%d\n", getMinModuloP(v, k, p));
  return 0;
}
