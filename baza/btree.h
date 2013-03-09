#ifndef BTREE_H_
#define BTREE_H_

#include "defs.h"

struct btree_node {
    struct btree_node* parent;
};

struct btree {
    size_t len;
    struct btree_node *root;
};

struct btree *create_btree();

#endif
