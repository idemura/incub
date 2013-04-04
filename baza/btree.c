#include "btree.h"
#include "stack.h"
#include <memory.h>

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

static iref btree_memory;
#define BTREE_ALIGN sizeof(iref)
#ifdef DEBUG
#define BTREE_PAD sizeof(iref)
#else
#define BTREE_PAD 0
#endif

static void *btree_alloc(iref size)
{
    size = (size + (BTREE_ALIGN - 1)) & ~(BTREE_ALIGN - 1);
    struct mem_block *mb = malloc(sizeof(struct mem_block) + size + BTREE_PAD);
    if (!mb) {
        return NULL;
    }
    mb->size = size;
    btree_memory += mb->size;
#ifdef DEBUG
    memset(mb->p, 0xcc, size + BTREE_PAD);
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
    for (iref i = 0; i < BTREE_PAD; ++i) {
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
    struct btree *bt = btree_alloc(sizeof(*bt));
    if (bt) {
        bt->min_keys = min_keys;
        bt->max_keys = 2 * min_keys;
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

static void btree_insert_in(struct btree_node *node, int i,
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

    bool found = false;
    struct btree_node *node = bt->root;
    int depth = bt->depth + 1;
    while (1) {
        jkey = btree_find_edge(node, key);
        found = (jkey != node->num && node->edge[jkey].key == key);
        depth -= 1;
        if (found || depth == 0) {
            break;
        }
        node = node->edge[jkey].ptr;
    }

    if (!found) {
        assert(depth == 0);
        *node_out = node;
        *jkey_out = jkey;
        return false;
    }

    if (depth != 0) {
        node = node->edge[jkey].ptr;
        while (1) {
            assert(node);
            jkey = node->num;
            depth -= 1;
            if (depth == 0) {
                break;
            }
            node = node->edge[jkey].ptr;
        }
    }

    assert(node);
    assert(depth == 0);
    *node_out = node;
    *jkey_out = jkey;
    return true;
}

void btree_insert(struct btree *bt, key_t key, void *value)
{
    // struct stack st;
    int jkey;
    struct btree_node *node;

    assert(value);
    if (!bt || !value) {
        return;
    }

    if (btree_locate(bt, key, &node, &jkey)) {
        node->edge[jkey].ptr = value;
        return;
    }

    struct btree_node *left = NULL;
    while (node && node->num == bt->max_keys) {
        assert(node->num % 2 == 0);
        int h = node->num / 2;
        left = btree_new_node(bt->max_keys);
        left->parent = node->parent;

        key_t new_key = key;
        // Virtually insert key in node `node` and find what key will be at
        // index `h`. This follows to 3 cases:
        if (jkey < h) {
            new_key = node->edge[h - 1].key;
            btree_copy_edge(left, node, 0, h - 1);
            btree_insert_in(left, jkey, key, value);
            btree_copy_edge(node, node, h, node->num);
        } else if (jkey == h) {
            btree_copy_edge(left, node, 0, h);
            left->edge[left->num].ptr = value;
            btree_copy_edge(node, node, h, node->num);
        } else {
            new_key = node->edge[h].key;
            btree_copy_edge(left, node, 0, h);
            btree_copy_edge(node, node, h + 1, node->num);
            btree_insert_in(node, jkey - h - 1, key, value);
        }
        assert(left->num == h);
        assert(node->num == h);
        value = left;
        key = new_key;
        node = node->parent;
    }

    if (!node) {
        node = bt->root;
        struct btree_node *new_node = btree_new_node(bt->max_keys);
        btree_insert_in(new_node, 0, key, value);
        new_node->edge[new_node->num].ptr = node;
        node->parent = new_node;
        left->parent = new_node;
        bt->root = new_node;
        bt->depth += 1;
    } else {
        btree_insert_in(node, jkey, key, value);
    }
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
