#ifndef TREE_H
#define TREE_H

#include "base.h"

template <class T>
struct Node {
    T key;
    Node *p = nullptr, *l = nullptr, *r = nullptr;

    Node(): key() {}
    explicit Node(T key): key(move(key)) {}
    bool leaf() const {
        return l == nullptr && r == nullptr;
    }
    bool root() const {
        return p == nullptr;
    }
};

template <class T>
Node<T> *make_node(T key) {
    return new Node<T>(move(key));
}

template <class T>
Node<T> *link(Node<T> *node, Node<T> *l, Node<T> *r) {
    node->l = l;
    if (l) l->p = node;
    node->r = r;
    if (r) r->p = node;
    return node;
}

// Inorder.
template <class T>
vector<T> walk(Node<T> *node) {
    vector<T> result;
    vector<Node<T> *> q;
    while (true) {
        if (node == nullptr) {
            result.push_back(T());
            if (q.empty()) break;
            node = q.back();
            q.pop_back();
        } else {
            result.push_back(node->key);
            q.push_back(node->r);
            node = node->l;
        }
    }
    return result;
}

template <class T>
void delete_tree(Node<T> *node) {
    if (node == nullptr) return;
    vector<Node<T> *> q{node};
    for (int i = 0; i < q.size(); i++) {
        auto n = q[i];
        if (n->l) q.push_back(n->l);
        if (n->r) q.push_back(n->r);
        delete n;
    }
}

template <class T>
Node<T> *find_root(Node<T> *node) {
    if (node == nullptr) return nullptr;
    while (node->p) {
        node = node->p;
    }
    return node;
}

template <class T>
void rotate(Node<T> *node) {
    auto up = node->p;
    if (up == nullptr) return;
    node->p = up->p;
    if (up->p) {
        if (up->p->l == up) {
            up->p->l = node;
        } else {
            up->p->r = node;
        }
    }
    up->p = node;
    if (up->l == node) {
        up->l = node->r;
        if (up->l) up->l->p = up;
        node->r = up;
    } else {
        up->r = node->l;
        if (up->r) up->r->p = up;
        node->l = up;
    }
}

// Splays node to the root.
template <class T>
void splay(Node<T> *node) {
    if (node == nullptr) return;
    while (node->p != nullptr) {
        if (node->p->p) {
            if ((node->p->l == node) == (node->p->p->l == node->p)) {
                // zig-zig
                rotate(node->p);
                rotate(node);
            } else {
                // zig-zag
                rotate(node);
                rotate(node);
            }
        } else {
            rotate(node);
        }
    }
}

// first keys <= node->k, second keys - the rest.
template <class T>
pair<Node<T> *, Node<T> *> splay_split(Node<T> *node) {
    splay(node);
    auto r = node->r;
    if (r) {
        node->r = r->p = nullptr;
    }
    return make_pair(node, r);
}

// Find maximum in the tree containing `node`.
template <class T>
Node<T> *find_max(Node<T> *node) {
    if (node == nullptr) return nullptr;
    node = find_root(node);
    while (node->r != nullptr) {
        node = node->r;
    }
    return node;
}

// Find minimum in the tree containing `node`.
template <class T>
Node<T> *find_min(Node<T> *node) {
    if (node == nullptr) return nullptr;
    node = find_root(node);
    while (node->l != nullptr) {
        node = node->l;
    }
    return node;
}

template <class T>
Node<T> *splay_join(Node<T> *l, Node<T> *r) {
    if (l) splay(l);
    if (r) splay(r);
    if (r == nullptr) return l;
    if (l == nullptr) return r;
    splay(find_max(l));
    l->r = r;
    r->p = l;
    return l;
}

template <class T>
Node<T> *find_node(Node<T> *node, T key) {
    while (node && node->key != key) {
        node = key < node->key ? node->l : node->r;
    }
    return node;
}

template <class T>
bool is_child(const Node<T> *node, const Node<T> *child, const Node<T> *other) {
    if (child != other) {
        cerr << "Failed " << (child == node->l ? "l" : "r") << " child" << endl;
        return false;
    }
    if (other != nullptr && other->p != node) {
        cerr << "Failed parent" << endl;
        return false;
    }
    return true;
}

template <class T>
bool is_l(const Node<T> *node, const Node<T> *other) {
    return is_child(node, node->l, other);
}
template <class T>
bool is_r(const Node<T> *node, const Node<T> *other) {
    return is_child(node, node->r, other);
}

#endif // TREE_H
