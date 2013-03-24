#include "btree.h"
#include "defs.h"
#include <stdio.h>
#include <memory.h>

// TODO:

// Ord in variable.

// insert_key: check max key num (memory overflow)

// btree: add allocator, because it allocs same block size always,
// possible optimizations.

struct mem_block {
    size_t size;
    char p[];
};

struct btree_node;

struct btree_branch {
    struct btree_node* ptr;
    key_t key;
};

struct btree_node {
    struct btree_node *parent;
    int num;
    struct btree_branch branch[];
};

struct btree {
    size_t size;
    int depth;
    int min_keys;
    int max_keys;
    struct btree_node *root;
};

static size_t btree_memory;

static void *btree_alloc(size_t size)
{
#ifdef DEBUG
    const size_t padding = sizeof(int);
#else
    const size_t padding = 0;
#endif
    size = (size + 3) & ~3; // Align on 4 byte boundary
    struct mem_block *mb = malloc(sizeof(struct mem_block) + size + padding);
    if (!mb) {
        return NULL;
    }
    mb->size = size;
    btree_memory += mb->size;
#ifdef DEBUG
    memset(mb->p, 0xcc, size + padding);
#endif
    return mb->p;
}

static void btree_free(void *p)
{
    if (!p) {
        return;
    }
    struct mem_block *mb = (void*)((char*)p - sizeof(struct mem_block));
    btree_memory -= mb->size;
#ifdef DEBUG
    assert(*(int*)(mb->p + mb->size) == 0xcccccccc);
#endif
    free(mb);
}

static size_t btree_node_size(int num_keys)
{
    return sizeof(struct btree_node) +
           sizeof(struct btree_branch) * num_keys +
           offsetof(struct btree_branch, key);
}

static struct btree_node *btree_new_node(int max_keys)
{
    struct btree_node* node = btree_alloc(btree_node_size(max_keys));
    node->parent = NULL;
    node->num = 0;
    node->branch[0].ptr = NULL;
    return node;
}

struct btree *btree_create(int min_keys)
{
    struct btree *bt = btree_alloc(sizeof(*bt));
    if (bt) {
        memset(bt, 0, sizeof(*bt));
        bt->min_keys = min_keys;
        bt->max_keys = 2 * min_keys;
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
            btree_free_node(node->branch[i].ptr, depth - 1);
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

static void btree_insert_in(struct btree_node *node, int i,
        key_t key, void *value)
{
    if (i != node->num && node->branch[i].key == key) {
        node->branch[i].ptr = value;
        return;
    }

    int j = node->num;
    node->branch[j + 1].ptr = node->branch[j].ptr;
    for (; j > i; --j) {
        node->branch[j] = node->branch[j - 1];
    }
    node->branch[i].ptr = value;
    node->branch[i].key = key;
    node->num += 1;
}

static int btree_find_branch(struct btree_node *node, key_t key)
{
    for (int i = 0; i < node->num; ++i) {
        if (key <= node->branch[i].key) {
            return i;
        }
    }
    return node->num;
}

static void btree_copy_branch(struct btree_node *dst, struct btree_node *src,
        int start, int end)
{
    memcpy(dst->branch, src->branch + start,
            sizeof(struct btree_branch) * (end - start));
}

void btree_insert(struct btree *bt, key_t key, void *value)
{
    int jkey;
    struct btree_node *node;

    assert(value);
    assert(bt);
    if (!bt || !value) {
        return;
    }

    if (bt->root == NULL) {
        node = btree_new_node(bt->max_keys);
        btree_insert_in(node, 0, key, value);
        bt->root = node;
        bt->size = 1;
        return;
    }

    node = bt->root;
    int depth = bt->depth;
    while (depth != 0) {
        jkey = btree_find_branch(node, key);
        node = node->branch[jkey].ptr;
        assert(node);
        depth -= 1;
    }

    jkey = btree_find_branch(node, key);
    if (jkey != node->num && node->branch[jkey].key == key) {
        node->branch[jkey].ptr = value;
        return;
    }

    while (node && node->num == bt->max_keys) {
        assert(node->num % 2 == 0);
        int h = node->num / 2;
        struct btree_node *left = btree_new_node(bt->max_keys);
        left->parent = node->parent;

        // Virtually insert key in node `node` and find what key will be in
        // at index `h`. This follows to 3 cases:
        if (jkey < h) {
            btree_copy_branch(left, node, 0, h - 1);
            left->branch[h - 1].ptr = NULL;
            left->num = h - 1;
            btree_insert_in(left, jkey, key, value);

            key = node->branch[h - 1].key;

            btree_copy_branch(node, node, h, node->num);
            node->branch[h].ptr = node->branch[node->num].ptr;
            node->num = h;
        } else if (jkey == h) {
            btree_copy_branch(left, node, 0, h);
            left->branch[h].ptr = NULL;

            btree_copy_branch(node, node, h, node->num);
            node->branch[h].ptr = node->branch[node->num].ptr;
            node->num = h;
        } else {
            btree_copy_branch(left, node, 0, h);
            left->branch[h].ptr = NULL;
            left->num = h;

            key = node->branch[h + 1].key;

            btree_copy_branch(node, node, h + 1, node->num);
            node->branch[h - 1].ptr = node->branch[node->num].ptr;
            node->num = h - 1;
            btree_insert_in(node, jkey - h - 1, key, value);
        }
        assert(left->num == h);
        assert(node->num == h);
        value = left;
        node = node->parent;
    }

    if (!node) {
        node = btree_new_node(bt->max_keys);
        btree_insert_in(node, 0, key, value);
        node->branch[node->num].ptr = bt->root;
        bt->root = node;
        bt->depth += 1;
    } else {
        btree_insert_in(node, jkey, key, value);
    }
    bt->size += 1;
}

void *btree_find(struct btree *bt, key_t key)
{
    int jkey;

    if (!bt || !bt->root) {
        return NULL;
    }

    struct btree_node *node = bt->root;
    int depth = bt->depth;
    for (; ; --depth) {
        jkey = btree_find_branch(node, key);
        if (jkey != node->num && node->branch[jkey].key == key) {
            break;
        }
        node = node->branch[jkey].ptr;
        if (depth == 0) {
            return NULL;
        }
    }

    for(; depth != 0; --depth) {
        assert(node);
        jkey = node->num;
        node = node->branch[jkey].ptr;
    }

    assert(node);
    assert(depth == 0);
    return node->branch[jkey].ptr;
}
