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
  float int_a, int_b;
  Node *l, *r;
  vector<Interval> ints;

  Node(float int_a, float int_b)
      : int_a(int_a), int_b(int_b), l(NULL), r(NULL) {
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

void testIntervals(const std::vector<Interval>& ints) {

}

int main()
{
#ifndef ONLINE_JUDGE
  freopen("in", "r", stdin);
#endif

  return 0;
}
