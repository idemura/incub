#include "btree.h"
#include "defs.h"
#include <stdio.h>

struct mem_block {
    size_t size;
    char p[];
};

struct btree_node;
struct btree_node_key;

struct btree_node_key
{
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
static const size_t ORD = 2;
static const size_t MAX_KEYS = 2*ORD-1;

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

static void btree_free_node(struct btree_node *node)
{
    if (!node) {
        return;
    }
    for (int i = 0; i < node->num; ++i) {
        btree_free_node(node->subnode[i].ptr);
    }
    btree_free(node);
}

void btree_destroy(struct btree *bt)
{
    if (!bt) {
        return;
    }
    btree_free_node(bt->root);
    btree_free(bt);
}

size_t btree_size(struct btree *bt)
{
    return bt->size;
}

static void btree_insert_at(struct btree_node *node, int i,
        int_key key, void *value)
{
    if (i != node->num) {
        for (int j = node->num; j > i; ++i) {
            node->subnode[j] = node->subnode[j - 1];
        }
    } else {
        node->subnode[i + 1].ptr = NULL;
    }
    node->subnode[i].key = key;
    node->subnode[i].ptr = NULL;
    node->num += 1;
}

void btree_insert(struct btree *bt, int_key key, void *value)
{
    if (!bt) {
        return;
    }
    if (bt->root == NULL) {
        struct btree_node *node = new_node();
        btree_insert_at(node, 0, key, value);
        bt->root = node;
        bt->size = 1;
        bt->depth = 0;
        return;
    }

    struct btree_node *node = bt->root;
    int depth = bt->depth;
    int i;
    while (depth != 0) {
        for (i = 0; i < node->num; ++i) {
            if (key <= node->subnode[i].key) {
                break;
            }
        }
        node = node->subnode[i].ptr;
        assert(node != NULL);
        depth -= 1;
    }

    while (node->num == MAX_KEYS) {
        assert(0);
    }

    btree_insert_at(node, i, key, value);
}

size_t btree_check_size(struct btree_node *node, int depth)
{
    assert(node);
    if (depth == 0) {
        printf("fast %d\n", node->num);
        return node->num;
    }
    size_t n = 0;
    for (int i = 0; i <= node->num; ++i) {
        n += btree_check_size(node->subnode[i].ptr, depth - 1);
    }
    printf("add %zu %d\n", n, node->num);
    return n + node->num;
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
    size_t real_size = btree_check_size(bt->root, bt->depth);
    if (real_size != bt->size) {
        fprintf(stderr, "Btree check: size %zu, real %zu\n", bt->size, real_size);
        return false;
    }
    return true;
#else
    return true;
#endif
}
