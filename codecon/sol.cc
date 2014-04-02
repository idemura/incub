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

struct Node {
  Node *l, *r;
  // int a, b;
  int c;

  Node(): l(), r(), c() {}
};

const int MAX_LEN = 1<<4;

void insertRec(Node *node, int a, int b, int x, int l)
{
  printf("insert at %p: a %d b %d l %d\n", node, a, b, l);
  if (!node) {
    printf("  return because is null.\n");
    return;
  }
  node->c += 1;
  int y = x + l - 1;
  printf("  x %d\n", x);
  printf("  y %d\n", y);
  // return;
  if (x == a && y == b) {
    printf("  match, return!\n");
    return;
  }
  int m = x + l / 2;
  // [x, m - 1] is left, [m, y] is right.
  printf("  left is %d - %d, right is %d - %d\n", x, m - 1, m, y);
  if (a <= m - 1 && b >= x) {
    printf("  insert %p: a=%d is on left\n", node, a);
    if (!node->l) {
      node->l = new Node();
      // node->l->p = node;
      printf("  make a new node %p\n", node->l);
    }
    insertRec(node->l, a, std::min(b, m - 1), x, l / 2);
  } else {
    printf("  %d %d is OUT of %d %d\n", a, b, x, m - 1);
  }
  if (b >= m && a <= y) {
    printf("  insert %p: b=%d is on right\n", node, b);
    if (!node->r) {
      node->r = new Node();
      // node->r->p = node;
      printf("  make a new node %p\n", node->r);
    }
    insertRec(node->r, std::max(a, m), b, m, l / 2);
  } else {
    printf("  %d %d is OUT of %d %d\n", a, b, m, y);
  }
  printf("%p return %d %d, left/right %p %p\n", node, a, b, node->l, node->r);
}

void insert(Node *root, int a, int b)
{
  insertRec(root, a, b, 0, MAX_LEN);
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
  printf("remove at %p: a %d b %d l %d\n", node, a, b, l);
  if (!node) {
    printf("  return because is null.\n");
    return;
  }
  node->c -= 1;
  printf("  new counter %d\n", node->c);
  if (node->c == 0) {
    printf("  delete tree %p\n", node);
    if (l == MAX_LEN) {
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
  printf("  x %d\n", x);
  printf("  y %d\n", y);
  // return;
  if (x == a && y == b) {
    printf("  match, return!\n");
    return;
  }
  int m = x + l / 2;
  printf("  left is %d - %d, right is %d - %d\n", x, m - 1, m, y);
  if (a <= m - 1 && b >= x) {
    printf("  remove %p: a=%d is on left\n", node, a);
    if (node->l) {
      removeRec(&node->l, a, std::min(b, m - 1), x, l / 2);
    }
  }
  if (b >= m) {
    printf("  remove %p: b=%d is on right\n", node, b);
    if (node->r) {
      removeRec(&node->r, std::max(a, m), b, m, l / 2);
    }
  }
  printf("%p return\n", node);
}

void remove(Node *root, int a, int b)
{
  Node *root_child_p = root;  // Preserve `root`.
  removeRec(&root_child_p, a, b, 0, MAX_LEN);
}

void printRec(Node *node, int x, int l, char *tab, int d)
{
  if (!node) {
    return;
  }
  tab[d] = 0;
  int y = x + l - 1;
  int m = x + l / 2;
  printf("%s%p x %d y %d c %d\n", tab, node, x, y, node->c);
  tab[d] = ' ';
  printRec(node->l, x, l / 2, tab, d + 1);
  printRec(node->r, m, l / 2, tab, d + 1);
}

void print(Node *node)
{
  char tab[40] = {};
  printRec(node, 0, MAX_LEN, tab, 0);
}

int main(int argc, char **argv)
{
// #ifndef ONLINE_JUDGE
//   freopen("in", "r", stdin);
// #endif
  auto *root = new Node();
  insert(root, 0, 4);
  printf("\n");
  print(root);
  printf("\n");

  insert(root, 4, 6);
  printf("\n");
  print(root);
  printf("\n");

  remove(root, 0, 4);
  printf("\n");
  print(root);
  printf("\n");

  remove(root, 4, 6);
  printf("\n");
  print(root);
  printf("\n");
  return 0;
}
