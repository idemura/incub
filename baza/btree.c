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

struct btree_node;

struct btree_edge {
    struct btree_node* ptr;
    vptr key;
};

struct btree_node {
    struct btree_node *parent;
    struct btree_node *next;
    struct btree_node *prev;
    int num;
    struct btree_edge edge[];
};

struct btree {
    compare_fn cmpf;
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

static struct btree_node *btree_new_node(int max_keys)
{
    struct btree_node *node = mem_alloc(btree_node_size(max_keys));
    node->parent = NULL;
    node->next = NULL;
    node->prev = NULL;
    node->num = 0;
    node->edge[0].ptr = NULL;
    return node;
}

struct btree *btree_create(compare_fn cmpf, int min_keys)
{
    if (min_keys < 2) {
        return NULL;
    }
    struct btree *bt = mem_alloc(sizeof(*bt));
    if (bt) {
        bt->cmpf = cmpf;
        bt->min_keys = min_keys - 1;
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
    vptr key, vptr value)
{
    int j = node->num;
    node->edge[j + 1].ptr = node->edge[j].ptr;
    for (; j > i; --j) {
        node->edge[j] = node->edge[j - 1];
    }
    node->edge[i].ptr = value;
    node->edge[i].key = key;
    node->num += 1;
}

static int btree_find_edge(compare_fn cmpf, struct btree_node *node, vptr key)
{
    int l = 0;
    int h = node->num;
    while (l < h) {
        int m = l + (h - l) / 2;
        assert(m < node->num);
        if (cmpf(key, node->edge[m].key) <= 0) {
            h = m;
        } else {
            l = m + 1;
        }
    }
    return l;
}

static void btree_copy_edges(struct btree_node *dst, struct btree_node *src,
    int start, int end)
{
    dst->num = end - start;
    memcpy(dst->edge, src->edge + start, sizeof(struct btree_edge) * dst->num);
    dst->edge[dst->num].ptr = src->edge[end].ptr;
}

static bool btree_locate(struct btree *bt, vptr key,
    struct btree_node **node_out, struct stack *st)
{
    int jkey;

    if (!bt) {
        *node_out = NULL;
        return false;
    }

    bool found = false;
    struct btree_node *node = bt->root;
    int depth = bt->depth + 1;
    while (1) {
        jkey = btree_find_edge(bt->cmpf, node, key);
        if (st) {
            stack_pushi(st, jkey);
        }
        found = jkey != node->num && bt->cmpf(node->edge[jkey].key, key) == 0;
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

static void btree_grow(struct btree *bt, vptr key, struct btree_node *node,
    struct btree_node *temp)
{
    bt->root = btree_new_node(bt->max_keys);
    bt->root->edge[0].ptr = node;
    bt->root->edge[0].key = key;
    bt->root->edge[1].ptr = temp;
    bt->root->num = 1;
    temp->parent = node->parent = bt->root;
    bt->depth += 1;
}

void btree_insert(struct btree *bt, vptr key, vptr value)
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
        vptr new_key = node->edge[h].key;

        // Copy upper `h` edges into temp node.
        struct btree_node *temp = btree_new_node(bt->max_keys);
        btree_copy_edges(temp, node, h + 1, node->num);
        temp->parent = node->parent;
        // Leave lower `h` edges in node.
        node->num = h;

        temp->prev = node;
        temp->next = node->next;
        node->next = temp;

        int jkey = stack_popi(&st);
        if (bt->cmpf(key, new_key) < 0) {
            assert(jkey <= h);
            btree_insert_in(node, jkey, key, value);
        } else {
            assert(jkey > h);
            jkey -= h + 1;
            btree_insert_in(temp, jkey, key, value);
        }

        key = new_key;

        if (node->parent) {
            jkey = stack_topi(&st);
            assert(node->parent->edge[jkey].ptr == node);
            node->parent->edge[jkey].ptr = temp;
            value = node;
            node = node->parent;
        } else {
            btree_grow(bt, key, node, temp);
            node = NULL;
            break;
        }
    }

    if (node) {
        btree_insert_in(node, stack_popi(&st), key, value);
    }

    bt->size += 1;
    stack_free(&st);
}

vptr btree_find(struct btree *bt, vptr key)
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
