#include <algorithm>
#include <map>
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
#include <unistd.h>

#define ARRAY_SIZEOF(A) (sizeof(A) / sizeof(A[0]))
#define INF 0x7fffffff
#define NON_COPYABLE(C) \
    C(const C&); \
    C& operator=(const C&);

typedef long long int lli;

struct Segm {
  int x, y;

  Segm(): x(), y() {}
  Segm(int x, int y): x(x), y(y) {}
};

struct Node {
  Node *l, *r;
  int c;

  Node(): l(), r(), c() {}
};

class StringPrinter {
  NON_COPYABLE(StringPrinter);
public:
  StringPrinter() {}
  void print(const char *fmt, ...);
  void flush(FILE *f);

private:
  std::string buf_;
};

void StringPrinter::print(const char *fmt, ...)
{
  char buf[80] = {};
  va_list ap;
  va_start(ap, fmt);
  vsnprintf(buf, sizeof buf, fmt, ap);
  va_end(ap);
  buf_.append(buf);
}

void StringPrinter::flush(FILE *f)
{
  fputs(buf_.c_str(), f);
  buf_.clear();
}

class Timer {
  NON_COPYABLE(Timer);
public:
  Timer(): total_(), count_() {}
  lli getTotal() const { return total_; }
  int getCount() const { return count_; }
  int getAverage() const {
    return count_? static_cast<int>(total_ / count_): -1;
  }

  class Scope {
    NON_COPYABLE(Scope);
  public:
    explicit Scope(Timer &timer): timer_(timer), start_(getAccurateTimer()) {}
    ~Scope() {
      timer_.Add(getAccurateTimer() - start_);
    }

  private:
    Timer &timer_;
    lli start_;
  };

private:
  // Thread timer in microseconds.
  static lli getAccurateTimer();
  void Add(lli t);

  lli total_, count_;
};

lli Timer::getAccurateTimer()
{
  struct timeval tv = {};
  gettimeofday(&tv, NULL);
  return static_cast<lli>(tv.tv_sec) * 1000000 + tv.tv_usec;
}

void Timer::Add(lli t)
{
  total_ += t;
  count_++;
}

template<class T, int N>
class ArenaAllocator {
  NON_COPYABLE(ArenaAllocator);
public:
  ArenaAllocator(): top_(), bottom_() {}

  ~ArenaAllocator()
  {
    for (size_t i = 0, n = chunks_.size(); i < n; i++) {
      operator delete(chunks_[i]);
    }
  }

  T* alloc()
  {
    char *p = NULL;
    if (free_ptrs_.empty()) {
      if (top_ == bottom_) {
        size_t chunk_size = sizeof(T) * N;
        chunks_.push_back(top_ = static_cast<char*>(operator new(chunk_size)));
        bottom_ = top_ + chunk_size;
      }
      p = top_;
      top_ += sizeof(T);
    } else {
      p = free_ptrs_[free_ptrs_.size() - 1];
      free_ptrs_.pop_back();
    }
    return new(p) T();
  }

  void dealloc(T* p)
  {
    p->T::~T();
    free_ptrs_.push_back(reinterpret_cast<char*>(p));
  }

private:
  std::vector<char*> free_ptrs_, chunks_;
  char *top_, *bottom_;
};


int max_len = 1<<30;  // Logically, this is a constant.
StringPrinter out;
ArenaAllocator<Node, 8000> al;

Node* makeNode()
{
  return al.alloc();
}

void deleteNode(Node *node)
{
  al.dealloc(node);
}

void insertRec(Node *node, int a, int b, int x, int l)
{
  if (!node) {
    return;
  }
  node->c += 1;
  int y = x + l - 1;
  if (x == a && y == b) {
    return;
  }
  int m = x + l / 2;
  // [x, m - 1] is left, [m, y] is right.
  if (a <= m - 1 && b >= x) {
    if (!node->l) {
      node->l = makeNode();
    }
    insertRec(node->l, a, std::min(b, m - 1), x, l / 2);
  }
  if (b >= m && a <= y) {
    if (!node->r) {
      node->r = makeNode();
    }
    insertRec(node->r, std::max(a, m), b, m, l / 2);
  }
}

Timer t_insert;
void insert(Node *root, int a, int b)
{
  Timer::Scope scope(t_insert);
  insertRec(root, a, b, 0, max_len);
}

void deleteTree(Node *node)
{
  if (!node) {
    return;
  }
  deleteTree(node->l);
  deleteTree(node->r);
  deleteNode(node);
}

void removeRec(Node **node_child_p, int a, int b, int x, int l)
{
  Node *node = *node_child_p;
  if (!node) {
    return;
  }
  node->c -= 1;
  if (node->c == 0) {
    if (l == max_len) {
      deleteTree(node->l);
      deleteTree(node->r);
      node->r = node->l = nullptr;
    } else {
      deleteTree(node);
      *node_child_p = nullptr;
    }
    return;
  }
  int y = x + l - 1;
  if (x == a && y == b) {
    return;
  }
  int m = x + l / 2;
  if (a <= m - 1 && b >= x) {
    if (node->l) {
      removeRec(&node->l, a, std::min(b, m - 1), x, l / 2);
    }
  }
  if (b >= m) {
    if (node->r) {
      removeRec(&node->r, std::max(a, m), b, m, l / 2);
    }
  }
}

Timer t_remove;
void remove(Node *root, int a, int b)
{
  Timer::Scope scope(t_remove);
  Node *root_child_p = root;  // Preserve `root`.
  removeRec(&root_child_p, a, b, 0, max_len);
}

int queryRec(Node *node, int n, int x, int l)
{
  if (!node) {
    return 0;
  }
  // Count how many exact coverages of this segment.
  int lc = node->l? node->l->c: 0;
  int rc = node->r? node->r->c: 0;
  int count = node->c - std::max(lc, rc);
  assert(count >= 0);

  int m = x + l / 2;
  if (n < m) {
    count += queryRec(node->l, n, x, l / 2);
  } else {
    count += queryRec(node->r, n, m, l / 2);
  }
  return count;
}

int query(Node *root, int n)
{
  return queryRec(root, n, 0, max_len);
}

int getMaxLen(int n)
{
  int max = 1;
  for (; max <= n; max <<= 1) {}
  return max;
}

void readLine(char *str, size_t str_max)
{
  char *p = str;
  for (int c = getchar_unlocked(); c != EOF && c != '\n';
       c = getchar_unlocked()) {
    *p++ = (char)c;
  }
  *p = 0;
}

int main(int argc, char **argv)
{
// #ifndef ONLINE_JUDGE
//   freopen("in", "r", stdin);
// #endif
  char line[120] = {};

  readLine(line, sizeof line);
  int n = 0, m = 0;
  sscanf(line, "%d%d", &n, &m);
  max_len = getMaxLen(n);

  Timer total;
  {
  Timer::Scope scope(total);
  auto *root = new Node();
  std::vector<Segm> segments;
  for (int i = 0; i < m; i++) {
    readLine(line, sizeof line);
    auto *p = line;
    int chars_consumed = 0;
    char op = 0;
    sscanf(p, "%c%n", &op, &chars_consumed);
    p += chars_consumed;
    switch (op) {
      case 'B': {
        int x = 0;
        sscanf(p, "%d", &x);
        out.print("%d\n", query(root, x));
      } break;
      case 'P': {
        int x = 0, y = 0;
        sscanf(p, "%d%d", &x, &y);
        insert(root, x, y);
        segments.push_back(Segm(x, y));
      } break;
      case 'M': {
        int j = 0, d = 0;
        sscanf(p, "%d%d", &j, &d);
        j--;  // Zero-based index.
        auto &s = segments[j];
        remove(root, s.x, s.y);
        s.x += d;
        s.y += d;
        insert(root, s.x, s.y);
      } break;
    }
  }
  deleteNode(root);
  }
  out.flush(stdout);
  printf("time ms %lld\n", total.getTotal() / 1000);
  printf("insert ms %lld\n", t_insert.getTotal() / 1000);
  printf("remove ms %lld\n", t_remove.getTotal() / 1000);
  return 0;
}
