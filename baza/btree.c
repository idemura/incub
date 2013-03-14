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
static const int ORD = BTREE_ORD;
static const int MAX_KEYS = 2 * BTREE_ORD - 1;

static void *btree_alloc(size_t size)
{
    printf("allocating %ld\n", size);
    printf("sizeof mem_block %zd\n", sizeof(struct mem_block));
    struct mem_block *mb = malloc(sizeof(struct mem_block) + size);
    if (!mb) {
        return NULL;
    }
    mb->size = 100;
    total_memory += size;
    printf("block %p, ret %p, diff %zd\n", mb, mb->p, mb->p - (char*)mb);
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
    printf("sizeof node %ld\n", sizeof(struct btree_node));
    size_t bytes = sizeof(struct btree_node) + subnode_size(MAX_KEYS);
    printf("subnode mem %ld\n", subnode_size(MAX_KEYS));
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
        printf("%p %d ", node->subnode[i].ptr, node->subnode[i].key);
    }
    printf("%p\n", node->subnode[node->num].ptr);
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
        printf("depth!\n");
        node = btree_find_subnode(node, key)->ptr;
        assert(node);
        depth -= 1;
    }

    btree_print_node(node);
    struct btree_subnode *pkey = btree_find_subnode(node, key);
    if (pkey != btree_last_subnode(node) && pkey->key == key) {
        pkey->ptr = value;
        return;
    }

    btree_insert_key(node, key)->ptr = value;
    bt->size += 1;
    printf("%ld\n", sizeof(struct btree_subnode));

    while (node->num == MAX_KEYS + 1) {
        printf("node to split:\n");
        btree_print_node(node);

        struct btree_node *parent = node->parent;
        assert(node->num % 2 == 0);
        int h = node->num / 2;
        printf("node to split 2:\n");
        btree_print_node(node);

        struct btree_node *right = new_node();
        printf("node to split 3:\n");
        btree_print_node(node);
        right->parent = parent;
        right->num = h;
        printf("%d\n", node->subnode[h].key);
        printf("%d\n", node->subnode[h + 1].key);
        printf("bytes to copy %zd\n", subnode_size(right->num));
        printf("node to split 4:\n");
        btree_print_node(node);
        memcpy(right->subnode, node->subnode + h, subnode_size(right->num));
        printf("right:\n");
        btree_print_node(right);

        node->num = h - 1;
        btree_last_subnode(node)->ptr = NULL; // ???
        int_key key = btree_last_subnode(node)->key;
        printf("left:\n");
        btree_print_node(node);

        if (!parent) {
            bt->root = new_node();
            bt->root->subnode[0].key = key;
            bt->root->subnode[0].ptr = node;
            bt->root->subnode[1].ptr = right;
            bt->root->parent = NULL;
            printf("new root:\n");
            btree_print_node(bt->root);
            node->parent = bt->root;
            right->parent = bt->root;
            bt->depth += 1;
            break;
        }
        printf("kokoko\n");
        // btree
        // Split by key
        // get another key/(value?) to insert up
        // if got root replace root in bt
        // assert(0);
    }
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
        depth -= 1;
    }

    struct btree_subnode *pkey = btree_find_subnode(node, key);
    if (pkey != btree_last_subnode(node) && pkey->key == key) {
        return pkey->ptr;
    } else {
        return NULL;
    }
}
