#include "btree.h"
#include "defs.h"
#include <stdio.h>
#include <memory.h>

// TODO:

// Ord in variable.

// insert_key: check max key num (memory overflow)

// btree: add allocator, because it allocs same block size always,
// possible optimizations.

#define BTREE_ORD 2

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

static void btree_print_node(struct btree_node *node)
{
    if (!node) {
        printf("btree_node NULL\n");
        return;
    }
    printf("btree_node %p: num=%i parent=%p\n", node, node->num, node->parent);
    if (node->num == 0) {
        return;
    }
    for (int i = 0; i < node->num; ++i) {
        printf("%p %ld ", node->branch[i].ptr, node->branch[i].key);
    }
    printf("%p\n", node->branch[node->num].ptr);
}

void btree_insert(struct btree *bt, key_t key, void *value)
{
    int jkey;

    assert(value);
    assert(bt);
    if (!bt || !value) {
        return;
    }

    if (bt->root == NULL) {
        struct btree_node *node = btree_new_node(bt->max_keys);
        btree_insert_in(node, 0, key, value);
        bt->root = node;
        bt->size = 1;
        return;
    }

    struct btree_node *node = bt->root;
    int depth = bt->depth;
    while (depth != 0) {
        printf("depth!\n");
        jkey = btree_find_branch(node, key);
        node = node->branch[jkey].ptr;
        assert(node);
        depth -= 1;
    }

    btree_print_node(node);
    jkey = btree_find_branch(node, key);
    if (jkey != node->num && node->branch[jkey].key == key) {
        node->branch[jkey].ptr = value;
        return;
    }

    while (node && node->num == bt->max_keys + 1) {
        int h = node->num / 2;
        struct btree_node* new = btree_new_node(bt->max_keys);
        new->parent = node->parent;
        new->num = h;
        node->num = h;

        if (key < node->branch[h].key) {
            memcpy(new->branch, node->branch, sizeof(struct btree_branch) * h);
            new->branch[new->num].ptr = NULL;
            memcpy(node->branch, node->branch + h, sizeof(struct btree_branch) * h);
            node->branch[node->num].ptr = node->branch[2 * node->num].ptr;
            new->num = h;
            new->branch[new->num].ptr = NULL;
            jkey = btree_find_branch(node, key);
            btree_insert_in(node, jkey, key, value);
            key = node->branch[h].key;
            new->num = h;
            value = new;
        } else {
        }
    }

    btree_insert_in(node, jkey, key, value);
    bt->size += 1;
}

void *btree_find(struct btree *bt, key_t key)
{
    int jkey;

    if (!bt || !bt->root) {
        return NULL;
    }

    struct btree_node *node = bt->root;
    bool found = false;
    for (int d = bt->depth; d != 0; --d) {
        jkey = btree_find_branch(node, key);
        if (jkey != node->num && node->branch[jkey].key == key) {
            // Find rightmost value?
            found = true;
        }
        node = node->branch[jkey].ptr;
    }

    jkey = btree_find_branch(node, key);
    if (found || (jkey != node->num && node->branch[jkey].key == key)) {
        return node->branch[jkey].ptr;
    } else {
        return NULL;
    }
}
