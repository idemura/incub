#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <memory.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))

typedef long long int lli;

struct node
{
    node *p, *c[2];
    lli key;

    node(): p(NULL), key(0)
    {
        c[0] = c[1] = NULL;
    }
};

node *tree_add(node *root, lli key)
{
    if (!root) {
        root = new node();
        root->key = key;
    } else {
        node *r = root, *p = root;
        int k = -1;
        do {
            k = key > p->key;
            r = p;
            p = p->c[k];
        } while (p);

        p = new node();
        p->key = key;
        p->p = root;
        r->c[k] = p;
    }
    return root;
}

node *tree_make(lli *keys, int n)
{
    node *r = NULL;
    for (int i = 0; i < n; i++) {
        r = tree_add(r, keys[i]);
    }
    return r;
}

void tree_free(node *t)
{
    if (!t) {
        return;
    }
    tree_free(t->c[0]);
    tree_free(t->c[1]);
    delete t;
}

void tree_check(node *t, lli *pmin, lli *pmax)
{
    lli r[2][2] = {};
    if (t->c[0]) {
        tree_check(t->c[0], &r[0][0], &r[0][1]);
        assert(r[0][1] <= t->key);
    }
    if (t->c[1]) {
        tree_check(t->c[1], &r[1][0], &r[1][1]);
        assert(r[1][0] >= t->key);
    }
    *pmin = t->c[0]? r[0][0]: t->key;
    *pmax = t->c[1]? r[1][1]: t->key;
}

void test1()
{
    lli a[] = { 5, 2, 4, 3, 1, 7, 9, 8, 6 };
    const int a_n = ARRAY_SIZEOF(a);
    node* t = tree_make(a, a_n);
    lli r[2] = {};
    tree_check(t, &r[0], &r[1]);
    assert(t->key == a[0]);
    tree_free(t);
    printf("test1 OK\n");
}

int main()
{
    test1();
    return 0;
}