#include "base.h"

// K and P are < and > comparable.
template <class K, class P>
struct Node {
    K k;
    P p;
    Node *l = nullptr, *r = nullptr;

    Node(K k, P p): k(k), p(p) {}
};

template <class K, class P>
Node<K, P> *merge(Node<K, P> *l, Node<K, P> *r) {
    Node<K, P> *root = nullptr;
    auto u = &root;
    while (l && r) {
        if (l->p > r->p) {
            *u = l;
            u = &l->r;
            l = *u;
        } else {
            *u = r;
            u = &r->l;
            r = *u;
        }
    }
    *u = l ? l : r;
    return root;
}

template <class K, class P>
Node<K, P> *merge_rec(Node<K, P> *l, Node<K, P> *r) {
    if (!l) return r;
    if (!r) return l;
    if (l->p > r->p) {
        l->r = merge_rec(l->r, r);
        return l;
    } else {
        r->l = merge_rec(l, r->l);
        return r;
    }
}

template <class K, class P>
void split(
        Node<K, P> *t,
        int k0,
        Node<K, P> **l,
        Node<K, P> **r,
        bool strict = false) {
    while (t) {
        if (t->k < k0 || (!strict && t->k == k0)) {
            *l = t;
            l = &t->r;
            t = *l;
        } else {
            *r = t;
            r = &t->l;
            t = *r;
        }
    }
    *l = *r = nullptr;
}

template <class K, class P>
void split_rec(Node<K, P> *t, int k0, Node<K, P> **l, Node<K, P> *r) {
    if (!t) {
        *l = *r = nullptr;
        return;
    }
    if (t->k <= k0) {
        split_rec(t->r, k0, l, r);
        t->r = *l;
        *l = t;
    } else {
        split_rec(t->l, k0, l, r);
        t->l = *r;
        *r = t;
    }
}

template <class K, class P>
void delete_treap(Node<K, P> *t) {
    if (t) {
        delete_treap(t->l);
        delete_treap(t->r);
        delete t;
    }
}

template <class K, class P>
void insert(Node<K, P> **t, int k, int p) {
    auto n = new Node<K, P>(k, p);
    if (!*t) {
        *t = n;
        return;
    }
    Node<K, P> *l = nullptr, *r = nullptr;
    split(*t, k, &l, &r);
    *t = merge(merge(l, n), r);
}

template <class K, class P>
void remove(Node<K, P> **t, int k) {
    if (!*t) return;
    Node<K, P> *l = nullptr, *r = nullptr, *q = nullptr;
    split(*t, k, &l, &r);
    split(l, k, &l, &q, true);
    delete_treap(q);
    *t = merge(l, r);
}

using NodeInt = Node<int, int>;

bool check_tree(NodeInt *t, int *kmin, int *kmax) {
    if (!t) return true;

    int bmin, bmax;
    if (t->l) {
        check_tree(t->l, &bmin, &bmax);
        if (!(bmax <= t->k)) {
            cout << "Tree failed at l of " << t->k << endl;
            return false;
        }
        *kmin = bmin;
    } else {
        *kmin = t->k;
    }

    if (t->r) {
        check_tree(t->r, &bmin, &bmax);
        if (!(bmin >= t->k)) {
            cout << "Tree failed at r of " << t->k << endl;
            return false;
        }
        *kmax = bmax;
    } else {
        *kmax = t->k;
    }

    return true;
}

bool check_heap(NodeInt *t) {
    if (!t) return true;

    if (t->l) {
        if (t->p <= t->l->p) {
            cout << "Heap failed on l at " << t->k << " p=" << t->p << endl;
            return false;
        }
        check_heap(t->l);
    }

    if (t->r) {
        if (t->p <= t->r->p) {
            cout << "Heap failed on r at " << t->k << " p=" << t->p << endl;
            return false;
        }
        check_heap(t->r);
    }

    return true;
}

bool check_treap(NodeInt *t) {
    int kmin = -1, kmax = -1;
    bool b1 = check_tree(t, &kmin, &kmax);
    bool b2 = check_heap(t);
    if (b1 && b2) {
        cout << "Treap is OK." << endl;
    }
    return b1 && b2;
}

void print_treap_node(NodeInt *t, int tab) {
    for (int i = 0; i < tab; i++) {
        cout << " ";
    }
    if (t) {
        cout << "k=" << t->k << " p=" << t->p << endl;
        print_treap_node(t->l, tab + 1);
        print_treap_node(t->r, tab + 1);
    } else {
        cout << "null" << endl;
    }
}

void print_treap(NodeInt *t) {
    if (!t) {
        cout << "Treap is null" << endl;
        return;
    }
    print_treap_node(t, 0);
}

int lcrn_seed = 13;
int lcrn_next() {
    lcrn_seed = 0x7fffffff & (lcrn_seed * 1103515245 + 12345);
    return lcrn_seed;
}

int rand_priority() {
    return lcrn_next() % 109;
}

int main() {
    NodeInt *t = nullptr;
    insert(&t, 10, rand_priority());
    insert(&t, 15, rand_priority());
    insert(&t, 20, rand_priority());
    insert(&t, 25, rand_priority());
    insert(&t, 30, rand_priority());
    insert(&t, 35, rand_priority());
    insert(&t, 40, rand_priority());
    insert(&t, 45, rand_priority());
    print_treap(t);
    check_treap(t);

    remove(&t, 30);
    print_treap(t);
    check_treap(t);
    return 0;
}
