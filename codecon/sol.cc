#include "base.h"

template<class T>
struct Node {
  T key;
  Node *p = nullptr, *l = nullptr, *r = nullptr;

  Node(): key() {}
  explicit Node(T key): key(key) {}
  bool leaf() const { return l == nullptr && r == nullptr; }
  bool root() const { return p == nullptr; }
};

template<class T>
Node<T>* make_node(T key) {
  return new Node<T>(key);
}

template<class T>
Node<T>* link(Node<T> *node, Node<T> *l, Node<T> *r) {
  node->l = l;
  if (l) l->p = node;
  node->r = r;
  if (r) r->p = node;
  return node;
}

template<class T>
vector<T> walk_depth(Node<T> *node) {
  vector<T> result;
  vector<Node<T>*> q;
  while (true) {
    if (node == nullptr) {
      result.push_back(0);
      if (q.empty()) break;
      node = q.back();
      q.pop_back();
    } else {
      result.push_back(node->key);
      q.push_back(node->r);
      node = node->l;
    }
  }
  return result;
}

// Breadth first search.
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
    if (up->p->l == up) {
      up->p->l = node;
    } else {
      up->p->r = node;
    }
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

// first keys <= node->k, second keys - the rest.
template<class T>
pair<Node<T>*, Node<T>*> split(Node<T> *node) {
  splay(node);
  auto r = node->r;
  if (r) {
    node->r = r->p = nullptr;
  }
  return make_pair(node, r);
}

template<class T>
Node<T>* find_max(Node<T>* node) {
  if (node == nullptr) return nullptr;
  node = find_root(node);
  while (node->r != nullptr) {
    node = node->r;
  }
  return node;
}

template<class T>
Node<T>* join(Node<T> *l, Node<T> *r) {
  if (l) splay(l);
  for(auto x : walk_depth(l)) cout<<x<<", ";
  cout<<endl;
  if (r) splay(r);
  for(auto x : walk_depth(r)) cout<<x<<", ";
  cout<<endl;
  if (r == nullptr) return l;
  if (l == nullptr) return r;
  CHECK(find_max(l)->key == 80);
  splay(find_max(l));
  l->r = r;
  r->p = l;
  return l;
}

template<class T>
Node<T>* find_node(Node<T> *node, T key) {
  while (node && node->key != key) {
    node = key < node->key ? node->l : node->r;
  }
  return node;
}

using NodeInt = Node<int>;

void test1() {
  auto n10 = make_node(10);
  auto n30 = make_node(30);
  auto n30 = make_node(40);
  auto n50 = make_node(50);
  auto n60 = make_node(60);
  auto n80 = make_node(80);
  auto n90 = make_node(90);
  link(n50, link(n30, n10, n40),
            link(n80, n60, n90));
  auto sub = split(n50);
  CHECK(sub.first == n50 && sub.second == n80);
  CHECK(n50.root());
  CHECK(n80.root());
  CHECK(walk_depth(n50) ==
        vector<int>({50, 30, 10, 0, 0, 40, 0, 0, 0}));
  CHECK(walk_depth(sub.second) ==
        vector<int>({80, 60, 0, 0, 90, 0, 0}));
  auto linked = join(sub.first, sub.second);
  CHECK(walk_depth(linked) ==
        vector<int>({50, 30, 10, 0, 0, 40, 0, 0, 80, 60, 0, 0, 90, 0, 0}));
  delete_tree(linked);
}

void test2() {
  auto t = make_node(50,
      make_node(30, make_node(10), make_node(40)),
      make_node(80, make_node(60), make_node(90)));
  auto split_at = find_node(t, 40);
  CHECK(split_at->key == 40);
  auto sub = split(split_at);
  CHECK(walk_depth(sub.first) ==
        vector<int>({40, 30, 10, 0, 0, 0, 0}));
  CHECK(walk_depth(sub.second) ==
        vector<int>({50, 0, 80, 60, 0, 0, 90, 0, 0}));
  auto linked = join(sub.first, sub.second);
  CHECK(walk_depth(linked) ==
        vector<int>({40, 30, 10, 0, 0, 0, 50, 0, 80, 60, 0, 0, 90, 0, 0}));
  delete_tree(linked);
}

void test3() {
  auto n50 =
  auto t = make_node(50,
      make_node(30, make_node(10), make_node(40)),
      make_node(80, make_node(60), make_node(90)));
  auto split_at = find_node;
  CHECK(split_at->key == 80);
  auto sub = split(split_at);
  CHECK(walk_depth(sub.first) ==
        vector<int>({80, 50, 30, 10, 0, 0, 40, 0, 0, 60, 0, 0, 0}));
  CHECK(walk_depth(sub.second) ==
        vector<int>({90, 0, 0}));
  // Move 60 to root.
  sub.first = find_node(sub.first, 60);
  splay(sub.first);
  CHECK(sub.first->key == 60);
  CHECK(sub.first->p == nullptr);
  CHECK(sub.first->r->key == 80);
  CHECK(walk_depth(sub.first) ==
        vector<int>({60, 50, 30, 10, 0, 0, 40, 0, 0, 0, 80, 0, 0}));
  auto linked = join(sub.first, sub.second);
  for(auto x : walk_depth(linked)) cout<<x<<", ";
  cout<<endl;
  CHECK(walk_depth(linked) ==
        vector<int>({80, 60, 50, 30, 10, 0, 0, 40, 0, 0, 0, 0, 90, 0, 0}));
  delete_tree(linked);

}

int main(int argc, char **argv) {
  ios_base::sync_with_stdio(false);
  // test1();
  // test2();
  test3();
  cout << "TESTS PASSED." << endl;
  return 0;
}
