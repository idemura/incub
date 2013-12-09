#include <algorithm>
#include <map>
#include <string>
#include <vector>
#include <stdlib.h>
#include <stdio.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define INF 0x7fffffff

using namespace std;

struct Node {
  int k;
  int p;
  Node *l, *r;

  Node(int k, int p) : k(k), p(p), l(nullptr), r(nullptr) { }
};

Node* merge(Node *L, Node *R)
{
  Node *root = nullptr, **U = &root;
  while (L && R) {
    if (L->p > R->p) {
      *U = L;
      U = &L->r;
      L = *U;
    } else {
      *U = R;
      U = &R->l;
      R = *U;
    }
  }
  *U = L? L: R;
  return root;
}

Node* merge_rec(Node *L, Node *R)
{
  if (!L) {
    return R;
  }
  if (!R) {
    return L;
  }
  if (L->p > R->p) {
    L->r = merge(L->r, R);
    return L;
  } else {
    R->l = merge(L, R->l);
    return R;
  }
}

void split(Node *T, int k0, Node **L, Node **R, bool strict = false)
{
  while (T) {
    if (T->k < k0 || (!strict && T->k == k0)) {
      *L = T;
      L = &T->r;
      T = *L;
    } else {
      *R = T;
      R = &T->l;
      T = *R;
    }
  }
  *L = *R = nullptr;
}

void split_rec(Node *T, int k0, Node **L, Node **R)
{
  if (!T) {
    *L = *R = nullptr;
    return;
  }
  if (T->k <= k0) {
    split(T->r, k0, L, R);
    T->r = *L;
    *L = T;
  } else {
    split(T->l, k0, L, R);
    T->l = *R;
    *R = T;
  }
}

void deleteTreap(Node *T)
{
  if (T) {
    printf("deleting %d\n", T->k);
    deleteTreap(T->l);
    deleteTreap(T->r);
    delete T;
  }
}

void insert(Node **T, int k, int p)
{
  Node *n = new Node(k, p);
  if (!*T) {
    *T = n;
    return;
  }
  Node *L = nullptr, *R = nullptr;
  split(*T, k, &L, &R);
  *T = merge(merge(L, n), R);
}

void remove(Node **T, int k)
{
  if (!*T) {
    return;
  }
  Node *L = nullptr, *R = nullptr;
  split(*T, k, &L, &R);
  Node *K = nullptr;  // Nodes exactly equal to `k`.
  split(L, k, &L, &K, true);
  deleteTreap(K);
  *T = merge(L, R);
}

bool checkTree(Node *T, int *kmin, int *kmax)
{
  if (!T) {
    return true;
  }

  int bmin, bmax;
  if (T->l) {
    checkTree(T->l, &bmin, &bmax);
    if (!(bmax <= T->k)) {
      printf("Tree failed at %d and %d: %d, %d\n", T->k, T->l->k, bmin, bmax);
      return false;
    }
    *kmin = bmin;
  }
  else {
    *kmin = T->k;
  }

  if (T->r) {
    checkTree(T->r, &bmin, &bmax);
    if (!(bmin >= T->k)) {
      printf("Tree failed at %d and %d: %d, %d\n", T->k, T->r->k, bmin, bmax);
      return false;
    }
    *kmax = bmax;
  } else {
    *kmax = T->k;
  }

  return true;
}

bool checkHeap(Node *T)
{
  if (!T) {
    return true;
  }

  if (T->l) {
    if (T->p <= T->l->p) {
      printf("Heap failed at %d(%d) and %d(%d)\n", T->k, T->p,
             T->l->k, T->l->p);
      return false;
    }
    checkHeap(T->l);
  }

  if (T->r) {
    if (T->p <= T->r->p) {
      printf("Heap failed at %d(%d) and %d(%d)\n", T->k, T->p,
             T->r->k, T->r->p);
      return false;
    }
    checkHeap(T->r);
  }

  return true;
}

bool checkTreap(Node *T)
{
  int kmin = -1, kmax = -1;
  bool b1 = checkTree(T, &kmin, &kmax);
  bool b2 = checkHeap(T);
  if (b1 && b2) {
    printf("Treap is OK.\n");
  }
  return b1 && b2;
}

void printTreapNode(Node *T, int tab)
{
  for (int i = 0; i < tab; i++) {
    printf(" ");
  }
  if (T) {
    printf("k %d p %d\n", T->k, T->p);
    printTreapNode(T->l, tab + 1);
    printTreapNode(T->r, tab + 1);
  } else {
    printf("null\n");
  }
}

void printTreap(Node *T)
{
  if (!T) {
    printf("Treap is null.\n");
    return;
  }
  printTreapNode(T, 0);
}

int lcrn_seed = 13;
int lcrnNext()
{
  lcrn_seed = 0x7fffffff & (lcrn_seed * 1103515245 + 12345);
  return lcrn_seed;
}

int randPriority()
{
  return lcrnNext() % 109;
}

int main()
{
  Node *T = nullptr;
  insert(&T, 10, randPriority());
  insert(&T, 15, randPriority());
  insert(&T, 20, randPriority());
  insert(&T, 25, randPriority());
  insert(&T, 30, randPriority());
  insert(&T, 35, randPriority());
  insert(&T, 40, randPriority());
  insert(&T, 45, randPriority());
  printTreap(T);
  checkTreap(T);

  remove(&T, 30);
  printTreap(T);
  checkTreap(T);
  return 0;
}
