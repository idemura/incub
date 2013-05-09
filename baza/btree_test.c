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

static const char *sbtree_file = "test_btree";

static int uint_cmp(vptr k1, vptr k2)
{
    return (uofs)k1 - (uofs)k2;
}

static uofs btree_count(struct btree_node *node, int depth)
{
    int i;
    assert(node);
    if (depth == 0) {
        return node->num;
    }
    uofs edge_sum = 0;
    for (i = 0; i <= node->num; ++i) {
        edge_sum += btree_count(node->edge[i].ptr, depth - 1);
    }
    return edge_sum + node->num;
}

static bool btree_check_keys_num(struct btree_node *node,
    int min_keys, int max_keys)
{
    return !node->parent || (min_keys <= node->num && node->num <= max_keys);
}

static bool btree_check_keys(compare_fn cmpf,
    struct btree_node *node, int depth,
    int min_keys,
    int max_keys,
    vptr max_keyval)
{
    int i;

    if (!node) {
        return true;
    }

    TEST_CHECKR(btree_check_keys_num(node, min_keys, max_keys),
        "Node %p num %i, must be %d-%d",
        (void*)node, node->num, min_keys, max_keys);

    for (i = 1; i < node->num; ++i) {
        TEST_CHECKR(cmpf(node->edge[i - 1].key, node->edge[i].key) < 0,
            "Node %p key order: %zu >= %zu @%i,%i",
            (void*)node,
            (uofs)node->edge[i - 1].key, (uofs)node->edge[i].key,
            i - 1, i);
    }

    for (i = 0; i < node->num; ++i) {
        TEST_CHECKR(cmpf(node->edge[i].key, max_keyval) < 0,
            "Node %p key bound: %zu >= %zu @%i",
            (void*)node, (uofs)node->edge[i].key, (uofs)max_keyval, i);
    }

    if (depth > 1) {
        for (i = 0; i <= node->num; ++i) {
            if (!btree_check_keys(cmpf, node->edge[i].ptr, depth - 1,
                    min_keys, max_keys,
                    i == node->num? max_keyval: node->edge[i].key)) {
                return false;
            }
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

    TEST_CHECKR(node->parent == parent,
        "Node %p parent %p (%p expected)",
        (void*)node, (void*)node->parent, (void*)parent);

    if (depth > 0) {
        int i;
        for (i = 0; i <= node->num; ++i) {
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
    TEST_CHECKR(size == bt->size,
        "Size %zu (%zu expected)",
        bt->size, size);

    if (!btree_check_parent(bt->root, bt->depth, NULL)) {
        return false;
    }
    const uofs max_keyval = 0x7fffffff;
    if (!btree_check_keys(bt->cmpf, bt->root, bt->depth,
            bt->min_keys, bt->max_keys, (vptr)max_keyval)) {
        return false;
    }
    return true;
}

static void btree_print_node(struct btree_node *node, int depth)
{
    if (!node) {
        return;
    }

    fprintf(test_out(), "depth %d\n", depth);
    btreedbg_node(node);

    if (depth > 0) {
        int i;
        for (i = 0; i <= node->num; ++i) {
            btree_print_node(node->edge[i].ptr, depth - 1);
        }
    }
}

static void btree_print(struct btree *bt)
{
    btree_print_node(bt->root, bt->depth);
}

static bool btree_check_print(struct btree *bt)
{
    if (btree_check(bt)) {
        return true;
    }
    btree_print(bt);
    return false;
}

static void btree_test_find_edge()
{
    struct btree_node* node = btree_new_node(4);
    node->edge[0].key = (vptr)10;
    node->edge[1].key = (vptr)20;
    node->edge[2].key = (vptr)30;
    node->edge[3].key = (vptr)40;
    node->num = 1;
    TEST_CHECK(btree_find_edge(uint_cmp, node, (vptr)5) == 0);
    TEST_CHECK(btree_find_edge(uint_cmp, node, (vptr)10) == 0);
    TEST_CHECK(btree_find_edge(uint_cmp, node, (vptr)15) == 1);
    node->num = 3;
    TEST_CHECK(btree_find_edge(uint_cmp, node, (vptr)5) == 0);
    TEST_CHECK(btree_find_edge(uint_cmp, node, (vptr)10) == 0);
    TEST_CHECK(btree_find_edge(uint_cmp, node, (vptr)15) == 1);
    TEST_CHECK(btree_find_edge(uint_cmp, node, (vptr)20) == 1);
    TEST_CHECK(btree_find_edge(uint_cmp, node, (vptr)25) == 2);
    TEST_CHECK(btree_find_edge(uint_cmp, node, (vptr)30) == 2);
    TEST_CHECK(btree_find_edge(uint_cmp, node, (vptr)35) == 3);
    node->num = 4;
    TEST_CHECK(btree_find_edge(uint_cmp, node, (vptr)5) == 0);
    TEST_CHECK(btree_find_edge(uint_cmp, node, (vptr)10) == 0);
    TEST_CHECK(btree_find_edge(uint_cmp, node, (vptr)15) == 1);
    TEST_CHECK(btree_find_edge(uint_cmp, node, (vptr)20) == 1);
    TEST_CHECK(btree_find_edge(uint_cmp, node, (vptr)25) == 2);
    TEST_CHECK(btree_find_edge(uint_cmp, node, (vptr)30) == 2);
    TEST_CHECK(btree_find_edge(uint_cmp, node, (vptr)35) == 3);
    TEST_CHECK(btree_find_edge(uint_cmp, node, (vptr)40) == 3);
    TEST_CHECK(btree_find_edge(uint_cmp, node, (vptr)45) == 4);
    mem_free(node);
}

static bool btree_test_case(vptr *keys, uofs keys_num)
{
    uofs i, j;
    struct btree_iter iter;

    struct btree *bt = btree_create(NULL, sbtree_file, uint_cmp, 2);
    for (i = 0; i < keys_num; ++i) {
        btree_insert(bt, keys[i], &keys[i]);
        TEST_CHECKR(btree_check_print(bt), NULL);
        TEST_CHECKR(btree_size(bt) == i + 1,
            "Size %zu, expected %zu", btree_size(bt), i + 1);
        for (j = 0; j <= i; ++j) {
            bool found = btree_find(bt, keys[j], &iter);
            TEST_CHECKR(found, "Key %zu not found", (uofs)keys[j]);
            if (found) {
                TEST_CHECKR(uint_cmp(btree_iter_key(&iter), keys[j]) == 0,
                    "Key found not equal: %zu (%zu expected)",
                    (uofs)btree_iter_key(&iter),
                    (uofs)keys[j]);
                TEST_CHECKR(btree_iter_value(&iter) == &keys[j],
                    "Value found not equal: %p (%p expected)",
                    btree_iter_value(&iter),
                    (void*)&keys[j]);
            }
        }
    }

    // Test iterator.
    // Stupid insertion sort.
    for (i = 0; i < keys_num; ++i) {
        uofs imin = i;
        for (j = i + 1; j < keys_num; ++j) {
            if (uint_cmp(keys[j], keys[imin]) < 0) {
                imin = j;
            }
        }
        vptr temp = keys[i];
        keys[i] = keys[imin];
        keys[imin] = temp;
    }

    TEST_CHECKM(btree_find(bt, keys[0], &iter),
        "Can't find first key %zu",
        (uofs)keys[0]);
    for (i = 0; i < keys_num; ++i) {
        TEST_CHECKM(uint_cmp(btree_iter_key(&iter), keys[i]) == 0,
            "Iterator key %zu (%zu expected)",
            (uofs)btree_iter_key(&iter), (uofs)keys[i]);
        if (i + 1 != keys_num) {
            TEST_CHECKM(btree_iter_next(&iter),
                "Can't go next");
        }
    }
    TEST_CHECKM(!btree_iter_next(&iter),
        "Iterator expected to reach end");

    for (i = 0; i < keys_num; ++i) {
        // fprintf(test_out(), "Update %zu\n", (uofs)keys[i]);
        vptr new_val = (vptr)(1000 + i);
        btree_insert(bt, keys[i], new_val);
        TEST_CHECKM(btree_find(bt, keys[i], &iter),
            "After update: can't find %zu",
            (uofs)keys[i]);
        TEST_CHECKM(uint_cmp(btree_iter_key(&iter), keys[i]) == 0,
            "After update: key %zu (%zu expected)",
            (uofs)btree_iter_key(&iter), (uofs)keys[i]);
        TEST_CHECKM(btree_iter_value(&iter) == new_val,
            "After update: value %p (%p expected)",
            btree_iter_value(&iter), new_val);
        TEST_CHECKM(btree_size(bt) == keys_num,
            "After update: size %zu (%zu expected)",
            btree_size(bt), keys_num);
    }

    btree_destroy(bt);
    return true;
}

void btree_test()
{
    struct btree *bt = NULL;

    test_begin("BTree");
    btree_test_find_edge();

    bt = btree_create(NULL, sbtree_file, uint_cmp, 2);
    btree_destroy(bt);

    // Test leaf insertion.
    uofs keys0[] = {
        20, 80, 60, 40
    };
    btree_test_case((vptr*)keys0, ARRAY_SIZEOF(keys0));

    // Test one level.
    uofs keys1[] = {
        20, 80, 60, 40, 10
    };
    btree_test_case((vptr*)keys1, ARRAY_SIZEOF(keys1));
    uofs keys2[] = {
        20, 80, 60, 40, 30
    };
    btree_test_case((vptr*)keys2, ARRAY_SIZEOF(keys2));
    uofs keys3[] = {
        20, 80, 60, 40, 50
    };
    btree_test_case((vptr*)keys3, ARRAY_SIZEOF(keys3));
    uofs keys4[] = {
        20, 80, 60, 40, 70
    };
    btree_test_case((vptr*)keys4, ARRAY_SIZEOF(keys4));
    uofs keys5[] = {
        20, 80, 60, 40, 90
    };
    btree_test_case((vptr*)keys5, ARRAY_SIZEOF(keys5));

    // Test two levels.
    uofs keys6[] = {
        20, 80, 60, 40, 10, 30, 70, 90, 15, 85, 32, 35, 62, 65, 38, 61, 36
    };
    btree_test_case((vptr*)keys6, ARRAY_SIZEOF(keys6));

    test_end();
}
