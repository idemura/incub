#include <algorithm>
#include <map>
#include <string>
#include <vector>
#include <stdio.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define INF 0x7fffffff

using namespace std;

struct Interval {
  float a, b;
  Interval(float a, float b) : a(a), b(b) { }
};

struct Node {
  Interval i;
  Node *l, *r;
  vector<Interval> ints;

  Node(float a, float b)
      : i(a, b), l(NULL), r(NULL) {
  }

  bool contains(const Interval& in) const {
    return i.a < in.a && in.b < i.b;
  }
};

Node* createTree() {
  Node* root = new Node(0, 8);
  root->l = new Node(0, 4);
  root->l->l = new Node(0, 2);
  root->l->l->l = new Node(0, 1);
  root->l->l->r = new Node(1, 2);
  root->l->r = new Node(2, 4);
  root->l->r->l = new Node(2, 3);
  root->l->r->r = new Node(3, 4);
  root->r = new Node(4, 8);
  root->r->l = new Node(4, 6);
  root->r->l->l = new Node(4, 5);
  root->r->l->r = new Node(5, 6);
  root->r->r = new Node(6, 8);
  root->r->r->l = new Node(6, 7);
  root->r->r->r = new Node(7, 8);
  return root;
}

void insert(Node *node, const Interval& in) {
  if (node->l && node->l->contains(in)) {
    insert(node->l, in);
    return;
  }
  if (node->r && node->r->contains(in)) {
    insert(node->r, in);
    return;
  }
  if (node->ints.size() > 0) {
    printf("in node %d %d intersects with:\n", in.a, in.b);
    for (size_t i = 0, n = node->ints.size(); i < n; i++) {
      printf("  %d %d\n", node->ints[i].a, node->ints[i].b);
    }
  }
  node->ints.push_back(in);
}

bool intervalIntersect(const Interval& i1, const Interval& i2) {
  return !(i1.b < i2.a || i1.a > i2.b);
}

void testIntervals(const std::vector<Interval>& ints) {
  for (size_t i = 1, n = ints.size(); i < n; i++) {
    for (size_t j = 0; j < i; j++) {
      if (intervalIntersect(ints[i], ints[j])) {
        printf("%d %d / %d %d\n", ints[i].a, ints[i].b, ints[j].a, ints[j].b);
      }
    }
  }

  printf("--------\n");

  Node *root = createTree();
  for (size_t i = 0, n = ints.size(); i < n; i++) {
    insert(ints[i], root);
  }


}

int main()
{
#ifndef ONLINE_JUDGE
  freopen("in", "r", stdin);
#endif

  return 0;
}
