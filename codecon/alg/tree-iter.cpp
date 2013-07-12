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

bool check_tree(node *t)
{
    lli min = 0, max = 0;
    if (t->c[0]) {
        check_tree(t->c[0], &min, &max);
        assert(max <= t->key);
    }
    if (t->c[1]) {
        check_tree(t->c[1], &min, &max);
        assert(min >= t->key);
    }
}

void test1()
{
    const lli[] = { 5, 2, 4, 3, 1, 7, 9, 8, 6 };
}

int main()
{
    test1();
    return 0;
}
