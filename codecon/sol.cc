#include "base.h"

// @T is a movable type. @Less is assumed a stateless predicate.
template<class T, class Less = less<T>>
class LeftistHeap {
public:
  using Value = T;

  explicit LeftistHeap() {}
  LeftistHeap(LeftistHeap &&other)
      : root_(other.root_), size_(other.size_) {
    other.make_null();
  }
  ~LeftistHeap() { del_node(root_); }
  LeftistHeap& operator=(LeftistHeap &&other) {
    root_ = other.root_;
    size_ = other.size_;
    other.make_null();
    return *this;
  }

  void merge(LeftistHeap *a, LeftistHeap* b) {
    del_node(root_);
    root_ = merge_node(a->root_, b->root_);
    size_ = a->size_ + b->size_;
    a->make_null();
    b->make_null();
  }

  Value pop() {
    const auto v = std::move(root_->v);
    auto xroot = root_;
    root_ = merge_node(root_->l, root_->r);
    delete xroot;
    size_--;
    return v;  // NRVO
  }

  void push(T v) {
    root_ = merge_node(root_, new Node(std::move(v)));
    size_++;
  }

  i64 size() const { return size_; }

  void clear() {
    del_node(root_);
    root_ = nullptr;
    size_ = 0;
  }

  void check() {
    if (root_ != nullptr) {
      check_node(root_->l, root_);
      check_node(root_->r, root_);
    }
  }

  string debug_string() {
    stringstream ss;
    ss<<"LeftistHeap: root="<<root_<<" size="<<size_<<"\n";
    print_node(root_, ss);
    return ss.str();
  }

private:
  struct Node {
    const Value v;
    Node *l = nullptr, *r = nullptr;
    i64 s = 1;
    explicit Node(Value v) : v(std::move(v)) {}
  };

  void make_null() {
    root_ = nullptr;
    size_ = 0;
  }

  static void del_node(Node *n) {
    if (nullptr == n) return;
    del_node(n->l);
    del_node(n->r);
  }

  static i64 get_s(Node *node) {
    return node == nullptr ? 0 : node->s;
  }

  static Node *merge_node(Node *a, Node *b) {
    if (a == nullptr) return b;
    if (b == nullptr) return a;
    Node *r = nullptr;
    if (less(a->v, b->v)) {
      a->r = merge_node(a->r, b);
      a->s = std::max(a->s, a->r->s + 1);
      r = a;
    } else {
      b->r = merge_node(b->r, a);
      b->s = std::max(b->s, b->r->s + 1);
      r = b;
    }
    if (get_s(r->r) > get_s(r->l)) std::swap(r->l, r->r);
    return r;
  }

  static void check_node(Node *n, Node *p) {
    if (n == nullptr) return;
    if (!less(p->v, n->v)) {
      cerr << "Heap property broken between "
           << "parent=" << p->v << " and child: " << n->v
           << endl;
      return;
    }
    check_node(n->l, n);
    check_node(n->r, n);
  }

  static void print_node(Node *n, stringstream &ss) {
    if (n == nullptr) return;
    ss<<n<<": v="<<n->v<<" s="<<n->s<< " l="<<n->l<<" r="<<n->r<<"\n";
    print_node(n->l, ss);
    print_node(n->r, ss);
  }

  static bool less(const Value &a, const Value &b) {
    return Less{}(a, b);
  }

  Node* root_ = nullptr;
  i64 size_ = 0;
  NON_COPYABLE(LeftistHeap);
};

int main(int argc, char **argv) {
  LeftistHeap<string> h;
  h.push("10");
  cout<<h.debug_string()<<endl;
  h.push("20");
  cout<<h.debug_string()<<endl;
  h.check();
  CHECK(2 == h.size());
  CHECK("10" == h.pop());
  CHECK("20" == h.pop());
  CHECK(0 == h.size());
  h.push("10");
  h.push("20");
  LeftistHeap<string> g;
  g.push("30");
  g.push("20");
  g.push("40");
  g.push("50");
  cout<<g.debug_string()<<endl;
  g.check();
  LeftistHeap<string> r;
  r.merge(&h, &g);
  CHECK(6 == r.size());
  cout << "TESTS PASSED." << endl;
  return 0;
}

