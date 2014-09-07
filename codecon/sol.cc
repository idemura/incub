#include <algorithm>
#include <functional>
#include <map>
#include <string>
#include <queue>
#include <vector>
#include <assert.h>
#include <ctype.h>
#include <limits.h>
#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define ARRAY_SIZEOF(A) (sizeof(A) / sizeof(A[0]))
#define INF 0x7fffffff
#define NON_COPYABLE(C) \
    C(const C&); \
    C& operator=(const C&);

using namespace std;

typedef long long int lli;

struct Rect {
  int x0, y0, x1, y1;
  Rect(): x0(), y0(), x1(), y1() {}
};

struct SweepLine {
  Rect *r;
  SweepLine(): r() {}
  int getY() const { return r->y0; }
  bool operator<(const SweepLine &other) const { return getY() < other.getY(); }
  bool operator>(const SweepLine &other) const { return getY() > other.getY(); }
};

struct Node {
  Node *lc, *rc;
  Rect *r;  // Highest rect that covers current range [x0, x1].
  int x0, x1;
  int h;  // Height in all the subtree.
  Node(): lc(), rc(), r(), x0(), x1(), h() {}
};

Node* buildTree(const vector<int> &xs, int i0, int i1) {
  assert(i1 - i0 >= 2);
  Node *n = new Node();
  n->x0 = xs[i0];
  n->x1 = xs[i1 - 1];
  if (i1 - i0 > 2) {
    int m = (i0 + i1) / 2;
    n->lc = buildTree(xs, i0, m + 1);
    n->rc = buildTree(xs, m, i1);
  }
  return n;
}

void deleteTree(Node *n) {
  if (!n) return;
  deleteTree(n->lc);
  deleteTree(n->rc);
  delete n;
}

int getMax(Node *root, int y) {
  return 0;
}

void insertRec(Node* n, Rect *r, int x0, int x1) {
  if (x1 <= n->x0 || x0 >= n->x1) {
    return;
  }
  if (r->y1 > n->h) {
    n->h = r->y1;
  }
  if (x0 <= n->x0 && n->x1 <= x1) {
    if (!n->r || r->y1 > n->r->y1) {
      n->r = r;
    }
  } else {
    insertRec(n->lc, r, x0, x1);
    insertRec(n->rc, r, x0, x1);
  }
}

void insert(Node* root, Rect *r) {
  insertRec(root, r, r->x0, r->x1);
}

void readAndSolve(Data &data) {
  int n, r_num;
  scanf("%d%d", &n, &r_num);
  vector<Rect> rects(r_num);
  for (int i = 0; i < rects.size(); i++) {
    Rect &r = rects[i];
    scanf("%d%d%d%d", &r.x0, &r.x1, &r.y0, &r.y1);
  }
  vector<SweepLine> lines(r_num);
  for (int i = 0; i < rects.size(); i++) {
    lines[i].y = rects[i].y0;
    lines[i].r = &rects[i];
  }
  sort(lines.begin(), lines.end());
  vector<int> xs(2 * r_num);
  for (int i = 0; i < rects.size(); i++) {
    xs[2 * i] = rects[i].x0;
    xs[2 * i + 1] = rects[i].x1;
  }
  sort(xs.begin(), xs.end());
  xs.erase(unique(xs.begin(), xs.end()), xs.end());
  Node *root = buildTree(xs);
  int sq_max = 0;
  for (int i = 0; i < lines.size(); i++) {
    if (lines[i].bottom) {
      int sq = getMax(root, lines[i].y);
      if (sq > sq_max) {
        sq_max = sq;
      }
      insert(root, lines[i].r);
    }
  }
  sq_max = std::max(sq_max, getMax(root, n));
  printf("%d\n", sq_max);
  deleteTree(root);
}

int main(int argc, char **argv) {
#ifndef ONLINE_JUDGE
  freopen("in", "r", stdin);
#endif

  int t = 0, r = 0, n = 0;
  scanf("%d", &t);
  for (int i = 0; i < t; i++) {
    Data data;
    readAndSolve(data);
  }
  return 0;
}
