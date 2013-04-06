/*
  Copyright 2013 Igor Demura

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
*/
#include "btree.h"
#include "stack.h"
#include <memory.h>
#include <stdio.h>

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
    uofs size;
    int min_keys;
    int max_keys;
};

static uofs btree_node_size(int num_keys)
{
    return sizeof(struct btree_node) +
           sizeof(struct btree_edge) * (uofs)num_keys +
           offsetof(struct btree_edge, key);
}

static struct btree_node *btree_temp(int max_keys)
{
    struct btree_node *node = mem_alloc(btree_node_size(max_keys));
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
    struct btree *bt = mem_alloc(sizeof(*bt));
    if (bt) {
        bt->min_keys = min_keys - 1;
        bt->max_keys = 2 * min_keys - 1;
        bt->root = btree_temp(bt->max_keys);
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
    mem_free(node);
}

void btree_destroy(struct btree *bt)
{
    if (!bt) {
        return;
    }
    btree_free_node(bt->root, bt->depth);
    mem_free(bt);
}

uofs btree_size(struct btree *bt)
{
    return bt->size;
}

static void btree_insert_in(struct btree_node *node, int i,
        key_t key, vptr value)
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

static void btree_copy_edges(struct btree_node *dst, struct btree_node *src,
        int start, int end)
{
    dst->num = end - start;
    memcpy(dst->edge, src->edge + start, sizeof(struct btree_edge) * dst->num);
    dst->edge[dst->num].ptr = src->edge[end].ptr;
}

static bool btree_locate(struct btree *bt, key_t key,
        struct btree_node **node_out, struct stack *st) {
    int jkey;

    if (!bt) {
        *node_out = NULL;
        return false;
    }

    bool found = false;
    struct btree_node *node = bt->root;
    int depth = bt->depth + 1;
    while (1) {
        jkey = btree_find_edge(node, key);
        if (st) {
            stack_pushi(st, jkey);
        }
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
        return false;
    }

    if (depth != 0) {
        node = node->edge[jkey].ptr;
        while (1) {
            assert(node);
            jkey = node->num;
            if (st) {
                stack_pushi(st, jkey);
            }
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
    return true;
}

void dbg_print(struct btree_node *node, const char *msg)
{
    printf("%p %s\n  | ", (void*)node, msg);
    for (int i = 0; i < node->num; ++i) {
        printf("%lu ", node->edge[i].key);
    }
    printf("|\n");
}

static void btree_grow(struct btree *bt, key_t key, struct btree_node *node,
        struct btree_node *temp)
{
    bt->root = btree_temp(bt->max_keys);
    bt->root->edge[0].ptr = node;
    bt->root->edge[0].key = key;
    bt->root->edge[1].ptr = temp;
    bt->root->num = 1;
    temp->parent = node->parent = bt->root;
    dbg_print(bt->root, "New root");
    bt->depth += 1;
}

void btree_insert(struct btree *bt, key_t key, vptr value)
{
    struct stack st;
    struct btree_node *node = NULL;

    assert(value);
    if (!bt || !value) {
        return;
    }

    stack_alloc(&st, 0);

    if (btree_locate(bt, key, &node, &st)) {
        node->edge[stack_popi(&st)].ptr = value;
        stack_free(&st);
        return;
    }

    assert(bt->max_keys % 2 == 1);
    const int h = bt->max_keys / 2;

    while (node->num == bt->max_keys) {
        printf("Key %lu, value %p, stack size %lu\n", key, value, stack_size(&st));
        dbg_print(node, "Split");
        if (node->parent) {
            dbg_print(node, "Parent");
        } else {
            printf("Upper node is NULL\n");
        }

        struct btree_node *temp = btree_temp(bt->max_keys);
        btree_copy_edges(temp, node, h + 1, node->num);
        temp->parent = node->parent;
        dbg_print(temp, "Temp");

        node->num = h;
        dbg_print(node, "Node after cut");
        key_t new_key = node->edge[h].key;
        printf("New key %lu\n", new_key);

        int jkey = stack_popi(&st);
        if (key < new_key) {
            // Insert left
            printf("Insert left @%d\n", jkey);
            assert(jkey <= h);
            btree_insert_in(node, jkey, key, value);
            dbg_print(node, "Node after insert (left)");
        } else {
            // Insert right
            printf("Insert right @%d -> %d\n", jkey, jkey - h - 1);
            assert(jkey > h);
            jkey -= h + 1;
            btree_insert_in(temp, jkey, key, value);
            dbg_print(temp, "Node after insert (left)");
        }

        key = new_key;

        if (node->parent) {
            jkey = stack_topi(&st);
            printf("Has parent, insert back @%d\n", jkey);
            dbg_print(node->parent, "Parent again");
            assert(node->parent->edge[jkey].ptr == node);
            node->parent->edge[jkey].ptr = temp;
            value = node;
            node = node->parent;
            printf("New value %p new node %p new key %lu\n", value, (void*)node, key);
        } else {
            printf("Grow tree\n");
            btree_grow(bt, key, node, temp);
            node = NULL;
            break;
        }
    }

    if (node) {
        printf("Final insert @%lu of %lu\n", stack_topi(&st), key);
        dbg_print(node, "Final insert");
        btree_insert_in(node, stack_popi(&st), key, value);
    }

    bt->size += 1;
    stack_free(&st);
}

vptr btree_find(struct btree *bt, key_t key)
{
    struct stack st;
    struct btree_node *node;

    if (!bt) {
        return NULL;
    }

    stack_alloc(&st, bt->depth);
    vptr value = NULL;
    if (btree_locate(bt, key, &node, &st)) {
        value = node->edge[stack_popi(&st)].ptr;
    }
    stack_free(&st);
    return value;
}
