#include "base.h"

// @T is a movable type.
template<class T, class Pred = less<T>>
class LeftistHeap {
public:
  using Value = T;

  explicit LeftistHeap(const Pred &p = Pred()) : p_(p) {}
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
    root_ = merge_node(a->root_, b->root_, p_);
    size_ = a->size_ + b->size_;
    a->make_null();
    b->make_null();
  }

  Value pop() {
    const auto v = std::move(root_->v);
    auto xroot = root_;
    root_ = merge_node(root_->l, root_->r, p_);
    delete xroot;
    size_--;
    return v;  // NRVO
  }

  void push(T v) {
    root_ = merge_node(root_, new Node(std::move(v)), p_);
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
      check_node(root_->l, root_, p_);
      check_node(root_->r, root_, p_);
    }
  }

  void print() {
    cout<<"LeftistHeap: root="<<root_<<" size="<<size_<<"\n";
    if (root_ == nullptr) return;
    print_node(root_);
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

  static i64 s_of(Node *node) {
    return node == nullptr ? 0 : node->s;
  }

  static Node *merge_node(Node *a, Node *b, const Pred &pred) {
    if (a == nullptr) return b;
    if (b == nullptr) return a;
    Node *r = nullptr;
    if (pred(a->v, b->v)) {
      a->r = merge_node(a->r, b, pred);
      a->s = std::max(a->s, a->r->s + 1);
      r = a;
    } else {
      b->r = merge_node(b->r, a, pred);
      b->s = std::max(b->s, b->r->s + 1);
      r = b;
    }
    if (s_of(r->r) > s_of(r->l)) std::swap(r->l, r->r);
    return r;
  }

  static void check_node(Node *n, Node *p, const Pred &pred) {
    if (n == nullptr) return;
    if (!pred(p->v, n->v)) {
      cerr << "Heap property broken between "
           << "parent=" << p->v << " and child: " << n->v
           << endl;
      return;
    }
    check_node(n, n->l, pred);
    check_node(n, n->r, pred);
  }

  static void print_node(Node *n) {
    if (n == nullptr) return;
    cout<<n<<": v="<<n->v<<" s="<<n->s<< " l="<<n->l<<" r="<<n->r<<"\n";
    print_node(n->l);
    print_node(n->r);
  }

  Node* root_ = nullptr;
  i64 size_ = 1;
  const Pred p_;
  NON_COPYABLE(LeftistHeap);
};

int main(int argc, char **argv) {
  LeftistHeap<string> h;
  h.push("10");
  h.print();
  h.push("20");
  h.print();
  cout<<"a"<<endl;
  h.check();
  cout<<"a"<<endl;
  CHECK("10" == h.pop());
  CHECK("20" == h.pop());
  cout << "TESTS PASSED." << endl;
  return 0;
}

