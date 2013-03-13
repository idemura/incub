#include "btree.h"
#include "defs.h"
#include <stdio.h>
#include <memory.h>

#define BTREE_ORD 2

struct mem_block {
    size_t size;
    char p[];
};

struct btree_node;
struct btree_subnode;

struct btree_subnode {
    struct btree_node* ptr;
    int_key key;
};

struct btree_node {
    struct btree_node *parent;
    int num;
    struct btree_subnode subnode[];
};

struct btree {
    size_t size;
    size_t depth;
    struct btree_node *root;
};

static size_t total_memory;
static const size_t ORD = BTREE_ORD;
static const size_t MAX_KEYS = 2 * BTREE_ORD - 1;

static void *btree_alloc(size_t size)
{
    struct mem_block *mb = malloc(sizeof(struct mem_block) + size);
    if (!mb) {
        return NULL;
    }
    mb->size = size;
    total_memory += size;
    return mb->p;
}

static void btree_free(void *p)
{
    struct mem_block *mb;
    if (!p) {
        return;
    }
    mb = (void*)((char*)p - sizeof(struct mem_block));
    total_memory -= mb->size;
    free(mb);
}

static size_t subnode_size(int num_keys)
{
    return sizeof(struct btree_subnode) * num_keys +
           offsetof(struct btree_subnode, key);
}

static struct btree_node *new_node()
{
    size_t bytes = sizeof(struct btree_node) + subnode_size(MAX_KEYS);
    struct btree_node* node = btree_alloc(bytes);
    node->parent = NULL;
    node->num = 0;
    return node;
}

size_t btree_memory()
{
    return total_memory;
}

struct btree *btree_create()
{
    struct btree *bt = btree_alloc(sizeof(*bt));
    if (bt) {
        memset(bt, 0, sizeof(*bt));
    }
    return bt;
}

static void btree_free_node(struct btree_node *node, int depth)
{
    if (!node) {
        return;
    }
    if (depth > 0) {
        for (int i = 0; i <= node->num; ++i) {
            btree_free_node(node->subnode[i].ptr, depth - 1);
        }
    }
    btree_free(node);
}

void btree_destroy(struct btree *bt)
{
    if (!bt) {
        return;
    }
    btree_free_node(bt->root, bt->depth);
    btree_free(bt);
}

size_t btree_size(struct btree *bt)
{
    return bt->size;
}

static struct btree_subnode *
btree_insert_key(struct btree_node *node, int_key key)
{
    int i;

    for (i = 0; i < node->num; ++i) {
        if (key <= node->subnode[i].key) {
            break;
        }
    }

    if (i != node->num && node->subnode[i].key == key) {
        return &node->subnode[i];
    }

    if (i != node->num) {
        for (int j = node->num + 1; j > i; --j) {
            node->subnode[j] = node->subnode[j - 1];
        }
    } else {
        node->subnode[i + 1].ptr = NULL;
    }
    node->subnode[i].key = key;
    node->subnode[i].ptr = NULL;
    node->num += 1;
    return &node->subnode[i];
}

static struct btree_subnode *
btree_last_subnode(struct btree_node *node)
{
    return &node->subnode[node->num];
}

static struct btree_subnode *
btree_find_subnode(struct btree_node *node, int_key key)
{
    int i;
    for (i = 0; i < node->num; ++i) {
        if (key <= node->subnode[i].key) {
            break;
        }
    }
    return &node->subnode[i];
}

void btree_insert(struct btree *bt, int_key key, void *value)
{
    assert(value);
    assert(bt);
    if (!bt || !value) {
        return;
    }
    if (bt->root == NULL) {
        struct btree_node *node = new_node();
        btree_insert_key(node, key)->ptr = value;
        bt->root = node;
        bt->size = 1;
        return;
    }

    struct btree_node *node = bt->root;
    int depth = bt->depth;
    while (depth != 0) {
        node = btree_find_subnode(node, key)->ptr;
        assert(node);
        depth -= 1;
    }

    struct btree_subnode *pkey = btree_find_subnode(node, key);
    if (pkey != btree_last_subnode(node) && pkey->key == key) {
        pkey->ptr = value;
        return;
    }

    while (node->num == MAX_KEYS) {
        struct btree_node *parent = node->parent;
        btree_insert_key(node, key)->ptr = value;
        assert(node->num % 2 == 0);
        struct btree_node *right = new_node();
        right->parent = parent;
        right->num = node->num >> 1;
        memcpy(right->subnode, node->subnode, subnode_size(right->num));
        node->num = subnode_size(right->num - 1);
        btree_last_subnode(node)->ptr = NULL;
        int_key key = btree_last_subnode(node)->key;

        if (parent) {
            // bt->root =
        } else {

        }

        // Split by key
        // get another key/(value?) to insert up
        // if got root replace root in bt
        assert(0);
    }

    btree_insert_key(node, key)->ptr = value;
    bt->size += 1;
}

void *btree_find(struct btree *bt, int_key key)
{
    if (!bt || !bt->root) {
        return NULL;
    }
    struct btree_node *node = bt->root;
    int depth = bt->depth;
    bool found = false;
    while (depth != 0) {
        struct btree_subnode *pkey = btree_find_subnode(node, key);
        if (pkey != btree_last_subnode(node) && pkey->key == key) {
            found = true;
        }
        node = pkey->ptr;
    }

    struct btree_subnode *pkey = btree_find_subnode(node, key);
    if (!pkey) {
        return NULL;
    } else {
        return pkey->ptr;
    }
}
