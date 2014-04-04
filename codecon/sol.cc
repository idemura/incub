#include <algorithm>
#include <map>
#include <string>
#include <vector>
#include <utility>
#include <assert.h>
#include <ctype.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define INF 0x7fffffff

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

// Logically, this is a constant.
int max_len = 1<<30;

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
      node->l = new Node();
    }
    insertRec(node->l, a, std::min(b, m - 1), x, l / 2);
  }
  if (b >= m && a <= y) {
    if (!node->r) {
      node->r = new Node();
    }
    insertRec(node->r, std::max(a, m), b, m, l / 2);
  }
}

void insert(Node *root, int a, int b)
{
  insertRec(root, a, b, 0, max_len);
}

void deleteTree(Node *node)
{
  if (!node) {
    return;
  }
  deleteTree(node->l);
  deleteTree(node->r);
  delete node;
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

void remove(Node *root, int a, int b)
{
  Node *root_child_p = root;  // Preserve `root`.
  removeRec(&root_child_p, a, b, 0, max_len);
}

int getCoverageRec(Node *node, int n, int x, int l)
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
    count += getCoverageRec(node->l, n, x, l / 2);
  } else {
    count += getCoverageRec(node->r, n, m, l / 2);
  }
  return count;
}

int getCoverage(Node *root, int n)
{
  return getCoverageRec(root, n, 0, max_len);
}

int getMaxLen(int n)
{
  int max = 1;
  for (; max <= n; max <<= 1) {}
  return max;
}

int main(int argc, char **argv)
{
// #ifndef ONLINE_JUDGE
//   freopen("in", "r", stdin);
// #endif
  char line[120] = {};

  fgets(line, sizeof line, stdin);
  int n = 0, m = 0;
  sscanf(line, "%d%d", &n, &m);
  max_len = getMaxLen(n);

  auto *root = new Node();
  std::vector<Segm> segments;
  for (int i = 0; i < m; i++) {
    auto *p = line;
    fgets(p, sizeof line, stdin);
    int chars_consumed = 0;
    char op = 0;
    sscanf(p, "%c%n", &op, &chars_consumed);
    p += chars_consumed;
    switch (op) {
      case 'B': {
        int x = 0;
        sscanf(p, "%d", &x);
        printf("%d\n", getCoverage(root, x));
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
  return 0;
}
