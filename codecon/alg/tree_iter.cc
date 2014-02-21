#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <memory.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))

typedef long long int lli;

struct Node
{
  Node *p, *c[2];
  lli key;

  Node(): p(), c(), key() {}
};

Node* treeAdd(Node *root, lli key)
{
  if (!root) {
    root = new Node();
    root->key = key;
  } else {
    Node *r = root, *p = root;
    int k = -1;
    do {
      k = key > p->key;
      r = p;
      p = p->c[k];
    } while (p);
    p = new Node();
    p->key = key;
    p->p = root;
    r->c[k] = p;
  }
  return root;
}

Node* treeMake(lli *keys, int n)
{
  Node *r = NULL;
  for (int i = 0; i < n; i++) {
    r = treeAdd(r, keys[i]);
  }
  return r;
}

void treeFree(Node *t)
{
  if (!t) {
    return;
  }
  treeFree(t->c[0]);
  treeFree(t->c[1]);
  delete t;
}

void treeCheck(Node *t, lli *pmin, lli *pmax)
{
  lli r[2][2] = {};
  if (t->c[0]) {
    treeCheck(t->c[0], &r[0][0], &r[0][1]);
    assert(r[0][1] <= t->key);
  }
  if (t->c[1]) {
    treeCheck(t->c[1], &r[1][0], &r[1][1]);
    assert(r[1][0] >= t->key);
  }
  *pmin = t->c[0]? r[0][0]: t->key;
  *pmax = t->c[1]? r[1][1]: t->key;
}

void test()
{
  lli a[] = { 5, 2, 4, 3, 1, 7, 9, 8, 6 };
  const int a_n = ARRAY_SIZEOF(a);
  Node* t = treeMake(a, a_n);
  lli r[2] = {};
  treeCheck(t, &r[0], &r[1]);
  assert(t->key == a[0]);
  treeFree(t);
  printf("test1 OK\n");
}

int main()
{
  test();
  return 0;
}
