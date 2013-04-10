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
#include "disk.h"
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
    struct disk_file *file;
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
           sizeof(struct btree_edge) * (uofs)(num_keys + 1);
}

static void btree_dbg_node(struct btree_node *node)
{
#ifdef DEBUG
    if (!node) {
        return;
    }
    log_print("%p [", (void*)node);
    for (int i = 0; i <= node->num; ++i) {
        log_print(" %zu", (uofs)node->edge[i].key);
    }
    log_print(" ] %d\n", node->num);
#endif
}

static struct btree_node *btree_new_node(int max_keys)
{
    struct btree_node *node = mem_alloc(btree_node_size(max_keys));
    node->parent = NULL;
    node->next = NULL;
    node->prev = NULL;
    node->num = 0;
    node->edge[0].ptr = NULL;
    node->edge[0].key = NULL;
    return node;
}

struct btree *btree_create(struct disk_file *file,
    compare_fn cmpf, int min_keys)
{
    if (min_keys < 2) {
        return NULL;
    }
    struct btree *bt = mem_alloc(sizeof(*bt));
    if (bt) {
        bt->file = file;
        bt->cmpf = cmpf;
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

static void btree_insert_at(struct btree_node *node, int i,
    vptr key, vptr value)
{
    for (int j = node->num + 1; j > i; --j) {
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
    memcpy(dst->edge, src->edge + start,
        sizeof(struct btree_edge) * (dst->num + 1));
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
    bt->root->edge[1].key = NULL;
    bt->root->num = 1;
    temp->parent = node->parent = bt->root;
    bt->depth += 1;
}

void btree_insert(struct btree *bt, vptr key, vptr value)
{
    struct stack st;
    struct btree_node *node = NULL;

    assert(key);
    assert(value);
    if (!bt || !key || !value) {
        return;
    }

    stack_alloc(&st, 0);
    if (btree_locate(bt, key, &node, &st)) {
        node->edge[stack_popi(&st)].ptr = value;
        stack_free(&st);
        return;
    }

    assert(bt->max_keys % 2 == 0);
    const int h = bt->max_keys / 2;

    while (node->num == bt->max_keys) {
        struct btree_node *temp = btree_new_node(bt->max_keys);
        temp->parent = node->parent;

        // Virtually insert in full node and split it on two. New key at
        // index `h`, three cases available:
        vptr split_key = NULL;
        int jkey = stack_popi(&st);
        if (jkey < h) {
            split_key = node->edge[h - 1].key;
            btree_copy_edges(temp, node, h, node->num);
            node->num = h - 1;
            btree_insert_at(node, jkey, key, value);
        } else if (jkey > h) {
            split_key = node->edge[h].key;
            btree_copy_edges(temp, node, h + 1, node->num);
            node->num = h;
            btree_insert_at(temp, jkey - h - 1, key, value);
        } else {
            split_key = key;
            btree_copy_edges(temp, node, h, node->num);
            node->num = h;
            node->edge[node->num].key = key;
            node->edge[node->num].ptr = value;
        }
        key = split_key;
        value = node;
        assert(key);
        assert(temp->num == h);
        assert(node->num == h);

        temp->prev = node;
        temp->next = node->next;
        node->next = temp;

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
        btree_insert_at(node, stack_popi(&st), key, value);
    }

    bt->size += 1;
    stack_free(&st);
}

bool btree_find(struct btree *bt, vptr key, struct btree_iter *iter)
{
    struct stack st;

    assert(iter);
    if (!bt) {
        iter->node = NULL;
        iter->j = 0;
        return false;
    }

    stack_alloc(&st, bt->depth);
    bool found = btree_locate(bt, key, &iter->node, &st);
    iter->j = stack_popi(&st);
    stack_free(&st);
    return found;
}

vptr btree_iter_key(struct btree_iter *iter)
{
    if (iter->node) {
        return iter->node->edge[iter->j].key;
    }
    return NULL;
}

vptr btree_iter_value(struct btree_iter *iter)
{
    if (iter->node) {
        return iter->node->edge[iter->j].ptr;
    }
    return NULL;
}

bool btree_iter_next(struct btree_iter *iter)
{
    if (iter->node == NULL) {
        return false;
    }
    if (iter->j < iter->node->num) {
        if (iter->node->edge[iter->j + 1].key) {
            iter->j++;
            return true;
        }
    }
    iter->node = iter->node->next;
    iter->j = 0;
    return iter->node != NULL;
}

bool btree_iter_prev(struct btree_iter *iter)
{
    return false;
}
