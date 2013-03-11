#include "btree.h"
#include "defs.h"
#include <stdio.h>

#define BTREE_ORD 2

struct mem_block {
    size_t size;
    char p[];
};

struct btree_node;
struct btree_node_key;

struct btree_node_key {
    struct btree_node* ptr;
    int_key key;
};

struct btree_node {
    struct btree_node *parent;
    int num;
    struct btree_node_key subnode[];
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

static struct btree_node *new_node()
{
    size_t bytes = sizeof(struct btree_node) +
                   sizeof(struct btree_node_key) * MAX_KEYS +
                   offsetof(struct btree_node_key, key);
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
        bt->size = 0;
        bt->root = NULL;
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

static struct btree_node_key *btree_insert_leaf(struct btree_node *node,
        int_key key)
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

static inline bool btree_last_subnode(struct btree_node *node)
{
    return &node->subnode[node->num];
}

static struct btree_node_key *btree_find_at_node(
        struct btree_node *node, int_key key)
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
        btree_insert_leaf(node, key)->ptr = value;
        bt->root = node;
        bt->size = 1;
        bt->depth = 0;
        return;
    }

    struct btree_node *node = bt->root;
    int depth = bt->depth;
    while (depth != 0) {
        struct btree_node_key *pkey = btree_find_at_node(node, key);
        node = pkey->ptr;
        assert(node);
        depth -= 1;
    }

    struct btree_node_key *pkey = btree_find_at_node(node, key);
    if (pkey != btree_last_subnode(node) && pkey->key == key) {
        pkey->ptr = value;
        return;
    }

    while (node->num == MAX_KEYS) {
        assert(0);
    }

    btree_insert_leaf(node, key)->ptr = value;
    bt->size += 1;
}

static size_t btree_count(struct btree_node *node, int depth)
{
#ifdef TEST
    assert(node);
    if (depth == 0) {
        return node->num;
    }
    size_t n = 0;
    for (int i = 0; i <= node->num; ++i) {
        n += btree_count(node->subnode[i].ptr, depth - 1);
    }
    return n + node->num;
#else
    return 0;
#endif
}

bool btree_check_keys(struct btree_node *node, int depth)
{
#ifdef TEST
    bool ok = true;
    for (int i = 1; i < node->num; ++i) {
        if (!(node->subnode[i - 1].key < node->subnode[i].key)) {
            ok = false;
            break;
        }
    }

    if (!ok) {
        fprintf(stderr, "Btree node keys:\n  ");
        for (int i = 0; i < node->num; ++i) {
            fprintf(stderr, "%i ", node->subnode[i].key);
        }
        fprintf(stderr, "\n");
        return false;
    }

    if (depth > 1) {
        for (int i = 0; i <= node->num; ++i) {
            if (!btree_check_keys(node->subnode[i].ptr, depth - 1)) {
                return false;
            }
        }
    }
    return true;
#else
    return true;
#endif
}

bool btree_check(struct btree *bt)
{
#ifdef TEST
    if (!bt) {
        return true;
    }
    if (!bt->root) {
        return bt->size == 0 && bt->depth == 0;
    }

    size_t size = btree_count(bt->root, bt->depth);
    if (size != bt->size) {
        fprintf(stderr, "Btree size: %zu != %zu\n", bt->size, size);
        return false;
    }
    if (!btree_check_keys(bt->root, bt->depth)) {
        return false;
    }
    return true;
#else
    return true;
#endif
}

void *btree_find(struct btree *bt, int_key key)
{
    if (!bt || !bt->root) {
        return NULL;
    }
    struct btree_node *node = bt->root;
    int depth = bt->depth;
    while (depth != 0) {
        node = btree_find_at_node(node, key)->ptr;
    }

    struct btree_node_key *pkey = btree_find_at_node(node, key);
    if (!pkey) {
        return NULL;
    } else {
        return pkey->ptr;
    }
}
