#include "btree.h"
#include "defs.h"
#include <memory.h>

// TODO:
// Add allocator, because it allocs same block size always, possible
// optimizations.

struct mem_block {
    iref size;
    unsigned char p[];
};

struct btree_node;

struct btree_edge {
    struct btree_node* ptr;
    key_t key;
};

struct btree_node {
    struct btree_node *parent;
    int num;
    struct btree_edge edge[];
};

struct btree {
    struct btree_node *root;
    int depth;
    iref size;
    int min_keys;
    int max_keys;
};

static const iref align = sizeof(iref);
static iref btree_memory;

static void *btree_alloc(iref size)
{
#ifdef DEBUG
    const iref padding = align;
#else
    const iref padding = 0;
#endif
    size = (size + (align - 1)) & ~(align - 1);
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
    for (int i = 0; i < align; ++i) {
        assert(mb->p[mb->size + i] == 0xcc);
    }
#endif
    free(mb);
}

static iref btree_node_size(int num_keys)
{
    return sizeof(struct btree_node) +
           sizeof(struct btree_edge) * (iref)num_keys +
           offsetof(struct btree_edge, key);
}

static struct btree_node *btree_new_node(int max_keys)
{
    struct btree_node* node = btree_alloc(btree_node_size(max_keys));
    node->parent = NULL;
    node->num = 0;
    node->edge[0].ptr = NULL;
    return node;
}

struct btree *btree_create(int min_keys)
{
    if (min_keys < 2) {
        return NULL;
    }
    struct btree *bt = btree_alloc(sizeof(*bt));
    if (bt) {
        bt->min_keys = min_keys;
        bt->max_keys = 2 * min_keys - 1;
        bt->root = btree_new_node(bt->max_keys);
        bt->size = 0;
        bt->depth = 0;
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
            btree_free_node(node->edge[i].ptr, depth - 1);
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

iref btree_size(struct btree *bt)
{
    return bt->size;
}

static void btree_insert_edge(struct btree_node *node, int i,
        key_t key, void *value)
{
    if (i != node->num && node->edge[i].key == key) {
        node->edge[i].ptr = value;
        return;
    }

    int j = node->num;
    node->edge[j + 1].ptr = node->edge[j].ptr;
    for (; j > i; --j) {
        node->edge[j] = node->edge[j - 1];
    }
    node->edge[i].ptr = value;
    node->edge[i].key = key;
    node->num += 1;
}

static int btree_find_edge(struct btree_node *node, key_t key)
{
    for (int i = 0; i < node->num; ++i) {
        if (key <= node->edge[i].key) {
            return i;
        }
    }
    return node->num;
}

static void btree_copy_edge(struct btree_node *dst, struct btree_node *src,
        int start, int end)
{
    dst->num = end - start;
    memcpy(dst->edge, src->edge + start, sizeof(struct btree_edge) * dst->num);
    dst->edge[dst->num].ptr = src->edge[end].ptr;
}

static bool btree_locate(struct btree *bt, key_t key,
        struct btree_node **node_out, int *jkey_out) {
    int jkey;

    if (!bt) {
        *node_out = NULL;
        *jkey_out = 0;
        return false;
    }

    struct btree_node *node = bt->root;
    int depth = bt->depth;
    while (1) {
        jkey = btree_find_edge(node, key);
        if (jkey != node->num && node->edge[jkey].key == key) {
            break;
        }
        if (depth == 0) {
            *node_out = node;
            *jkey_out = jkey;
            return false;
        }
        depth -= 1;
        node = node->edge[jkey].ptr;
    }

    if (depth != 0) {
        node = node->edge[jkey].ptr;
        while (1) {
            assert(node);
            depth -= 1;
            if (depth == 0) {
                jkey = node->num;
                break;
            }
            node = node->edge[node->num].ptr;
        }
    }

    assert(node);
    assert(depth == 0);
    *node_out = node;
    *jkey_out = jkey;
    return true;
}

static void btree_split_node(struct btree_node* node, int parent_index)
{
    // split and insert at jkey
}

void btree_insert(struct btree *bt, key_t key, void *value)
{
    assert(value);
    assert(bt);
    if (!bt || !value) {
        return;
    }

    int jkey = 0;
    struct btree_node *node;
    // struct btree_node *next_node = NULL;
    int depth = bt->depth;
    int edge_i = 0;

    node = bt->root;
    // const int left_index = bt->max_keys / 2 + 1;

    while (1) {
        if (node->num == bt->max_keys) {
            if (node->parent == NULL) {
                bt->root = btree_new_node(bt->max_keys);
                // split, set new root, inc depth
                bt->depth += 1;
                assert(edge_i == 0);
                node->parent = bt->root;
            }

            // find where to go next
            btree_split_node(node, jkey);

            int right = key > node->parent->edge[jkey].key;
            node = node->parent->edge[jkey + right].ptr;
        }

        jkey = btree_find_edge(node, key);
        if (jkey < node->num && node->edge[jkey].key == key) {
            // Update
            assert(false);
        }

        if (depth == 0) {
            break;
        }

        depth -= 1;
        edge_i = jkey;
        node = node->edge[jkey].ptr;
    }

    btree_insert_edge(node, jkey, key, value);
    bt->size += 1;
}

void *btree_find(struct btree *bt, key_t key)
{
    int jkey;
    struct btree_node *node;

    if (!bt) {
        return NULL;
    }

    if (btree_locate(bt, key, &node, &jkey)) {
        return node->edge[jkey].ptr;
    } else {
        return NULL;
    }
}
