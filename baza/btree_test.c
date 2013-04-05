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
#include "btree.c"
#include "test.h"

static uofs btree_count(struct btree_node *node, int depth)
{
    assert(node);
    if (depth == 0) {
        return node->num;
    }
    uofs edge_sum = 0;
    for (int i = 0; i <= node->num; ++i) {
        edge_sum += btree_count(node->edge[i].ptr, depth - 1);
    }
    return edge_sum + node->num;
}

static bool btree_check_keys_num(struct btree_node *node,
        int min_keys, int max_keys)
{
    return !node->parent || (min_keys <= node->num && node->num <= max_keys);
}

static bool btree_check_keys(struct btree_node *node,
        int depth,
        int min_keys, int max_keys,
        key_t max_key_value)
{
    if (!node) {
        return true;
    }
    if (!btree_check_keys_num(node, min_keys, max_keys)) {
        fprintf(test_out(), "Node %p num %i, must be %d-%d\n",
                (void*)node, node->num, min_keys, max_keys);
        return false;
    }

    int i;
    for (i = 1; i < node->num; ++i) {
        if (!(node->edge[i - 1].key < node->edge[i].key)) {
            fprintf(test_out(), "Node %p key order: %li >= %li @%i,%i\n",
                    (void*)node, node->edge[i - 1].key, node->edge[i].key,
                    i - 1, i);
            break;
        }
    }

    for (i = 0; i < node->num; ++i) {
        if (!(node->edge[i].key < max_key_value)) {
            fprintf(test_out(), "Node %p key bound: %li >= %li @%i\n",
                    (void*)node, node->edge[i].key, max_key_value, i);
            break;
        }
    }

    if (depth > 1) {
        for (i = 0; i < node->num; ++i) {
            if (!btree_check_keys(node->edge[i].ptr, depth - 1,
                    min_keys, max_keys, node->edge[i].key)) {
                return false;
            }
        }
        if (!btree_check_keys(node->edge[node->num].ptr, depth - 1,
                min_keys, max_keys, max_key_value)) {
            return false;
        }
    }
    return true;
}

static bool btree_check_parent(struct btree_node *node, int depth,
        struct btree_node *parent)
{
    if (!node) {
        return true;
    }
    if (node->parent != parent) {
        fprintf(test_out(), "Node %p parent %p != %p\n",
                (void*)node, (void*)node->parent, (void*)parent);
        return false;
    }

    if (depth > 1) {
        for (int i = 0; i <= node->num; ++i) {
            if (!btree_check_parent(node->edge[i].ptr, depth - 1, node)) {
                return false;
            }
        }
    }
    return true;
}

static bool btree_check(struct btree *bt)
{
    if (!bt) {
        return true;
    }
    if (!bt->root) {
        return bt->size == 0 && bt->depth == 0;
    }

    uofs size = btree_count(bt->root, bt->depth);
    if (size != bt->size) {
        fprintf(test_out(), "Btree size %zu != %zu\n", bt->size, size);
        return false;
    }
    if (!btree_check_parent(bt->root, bt->depth, NULL)) {
        return false;
    }
    if (!btree_check_keys(bt->root, bt->depth, bt->min_keys, bt->max_keys,
            0x7fffffff)) {
        return false;
    }
    return true;
}

static void btree_print(struct btree_node *node, int depth)
{
    if (!node) {
        return;
    }

    fprintf(test_out(), "%p parent=%p num=%i depth=%i\n",
            (void*)node->parent, (void*)node, node->num, depth);
    if (node->num == 0) {
        return;
    }
    fprintf(test_out(), "   ");
    for (int i = 0; i < node->num; ++i) {
        fprintf(test_out(), "%p %li ", (void*)node->edge[i].ptr,
                node->edge[i].key);
    }
    fprintf(test_out(), "%p\n", (void*)node->edge[node->num].ptr);
    if (depth > 0) {
        for (int i = 0; i <= node->num; ++i) {
            btree_print(node->edge[node->num].ptr, depth - 1);
        }
    }
}

static bool btree_check_print(struct btree *bt)
{
    if (btree_check(bt)) {
        return true;
    }
    btree_print(bt->root, bt->depth);
    return false;
}

static void btree_test_insert(key_t *keys, uofs keys_num)
{
    uofs mem = mem_total();

    struct btree *bt = btree_create(2);
    for (uofs i = 0; i < keys_num; ++i) {
        // fprintf(test_out(), "Insert %li\n", keys[i]);
        btree_insert(bt, keys[i], &keys[i]);
        TEST_CHECK(btree_check_print(bt));
        TEST_CHECK(btree_size(bt) == i + 1);
        for (uofs j = 0; j < i; ++j) {
            vptr value = btree_find(bt, keys[j]);
            if (value != &keys[j]) {
                fprintf(test_out(), "Key %li not found\n", keys[j]);
            }
            TEST_CHECK(value == &keys[j]);
        }
    }

    key_t *new_val = mem_alloc(keys_num * sizeof(key_t));
    memset(new_val, 0, keys_num * sizeof(key_t));
    for (uofs i = 0; i < keys_num; ++i) {
        // fprintf(test_out(), "Update %li\n", keys[i]);
        btree_insert(bt, keys[i], &new_val[i]);
        TEST_CHECK(btree_find(bt, keys[i]) == &new_val[i]);
        TEST_CHECK(btree_size(bt) == keys_num);
    }
    mem_free(new_val);

    btree_destroy(bt);
    TEST_CHECK(mem_total() == mem);
}

void btree_test()
{
    struct btree *bt = NULL;

    test_begin("BTree");

    bt = btree_create(2);
    btree_destroy(bt);

    key_t keys0[] = {
        10, 20, 15
    };
    btree_test_insert(keys0, ARRAY_SIZE(keys0));
    key_t keys1[] = {
        10, 20, 15, 7
    };
    btree_test_insert(keys1, ARRAY_SIZE(keys1));
    key_t keys2[] = {
        10, 20, 15, 13
    };
    btree_test_insert(keys2, ARRAY_SIZE(keys2));
    key_t keys3[] = {
        10, 20, 15, 17
    };
    btree_test_insert(keys3, ARRAY_SIZE(keys3));
    key_t keys4[] = {
        10, 20, 15, 23
    };
    btree_test_insert(keys4, ARRAY_SIZE(keys4));

    test_end();
}
