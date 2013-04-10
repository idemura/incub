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

static int uint_cmp(vptr k1, vptr k2)
{
    return (uofs)k1 - (uofs)k2;
}

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

static bool btree_check_keys(compare_fn cmpf,
    struct btree_node *node, int depth,
    int min_keys,
    int max_keys,
    vptr max_keyval)
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
        if (!(cmpf(node->edge[i - 1].key, node->edge[i].key) < 0)) {
            fprintf(test_out(), "Node %p key order: %zu >= %zu @%i,%i\n",
                (void*)node,
                (uofs)node->edge[i - 1].key, (uofs)node->edge[i].key,
                i - 1, i);
            return false;
        }
    }

    for (i = 0; i < node->num; ++i) {
        if (!(cmpf(node->edge[i].key, max_keyval) < 0)) {
            fprintf(test_out(), "Node %p key bound: %zu >= %zu @%i\n",
                (void*)node, (uofs)node->edge[i].key, (uofs)max_keyval, i);
            return false;
        }
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
    if (node->parent != parent) {
        fprintf(test_out(), "Node %p parent %p != %p\n",
            (void*)node, (void*)node->parent, (void*)parent);
        return false;
    }

    if (depth > 1) {
        for (int i = 0; i < node->num; ++i) {
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
    if (!btree_check_keys(bt->cmpf, bt->root, bt->depth,
            bt->min_keys, bt->max_keys, (vptr)0x7fffffff)) {
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
    for (int i = 0; i <= node->num; ++i) {
        fprintf(test_out(), "%p %zu ", (void*)node->edge[i].ptr,
            (uofs)node->edge[i].key);
    }
    fprintf(test_out(), "\n");
    if (depth > 0) {
        for (int i = 0; i <= node->num; ++i) {
            btree_print(node->edge[i].ptr, depth - 1);
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

static void btree_test_case(vptr *keys, uofs keys_num)
{
    struct btree_iter iter;

    struct btree *bt = btree_create(NULL, uint_cmp, 2);
    for (uofs i = 0; i < keys_num; ++i) {
        // fprintf(test_out(), "Insert %zu\n", (uofs)keys[i]);
        btree_insert(bt, keys[i], &keys[i]);
        TEST_CHECK(btree_check_print(bt));
        TEST_CHECK(btree_size(bt) == i + 1);
        for (uofs j = 0; j <= i; ++j) {
            bool found = btree_find(bt, keys[j], &iter);
            TEST_CHECK(found);
            TEST_CHECK(uint_cmp(btree_iter_key(&iter), keys[j]) == 0);
            TEST_CHECK(btree_iter_value(&iter) == &keys[j]);
        }
    }

    // Test iterator.
    // Stupid insertion sort.
    for (uofs i = 0; i < keys_num; ++i) {
        uofs imin = i;
        for (int j = i + 1; j < keys_num; ++j) {
            if (uint_cmp(keys[j], keys[imin]) < 0) {
                imin = j;
            }
        }
        vptr temp = keys[i];
        keys[i] = keys[imin];
        keys[imin] = temp;
    }

    TEST_CHECK(btree_find(bt, keys[0], &iter));
    for (uofs i = 0; i < keys_num; ++i) {
        TEST_CHECK(uint_cmp(btree_iter_key(&iter), keys[i]) == 0);
        if (i + 1 != keys_num) {
            TEST_CHECK(btree_iter_next(&iter));
        }
    }
    TEST_CHECK(!btree_iter_next(&iter));

    for (uofs i = 0; i < keys_num; ++i) {
        // fprintf(test_out(), "Update %zu\n", (uofs)keys[i]);
        vptr new_val = (vptr)(1000 + i);
        btree_insert(bt, keys[i], new_val);
        TEST_CHECK(btree_find(bt, keys[i], &iter));
        TEST_CHECK(uint_cmp(btree_iter_key(&iter), keys[i]) == 0);
        TEST_CHECK(btree_iter_value(&iter) == new_val);
        TEST_CHECK(btree_size(bt) == keys_num);
    }

    btree_destroy(bt);
}

void btree_test()
{
    struct btree *bt = NULL;

    test_begin("BTree");
    btree_test_find_edge();

    bt = btree_create(NULL, uint_cmp, 2);
    btree_destroy(bt);

    // Test leaf insertion.
    uofs keys0[] = {
        20, 80, 60, 40
    };
    btree_test_case((vptr*)keys0, ARRAY_SIZE(keys0));

    // Test grow one level.
    uofs keys1[] = {
        20, 80, 60, 40, 10
    };
    btree_test_case((vptr*)keys1, ARRAY_SIZE(keys1));
    uofs keys2[] = {
        20, 80, 60, 40, 30
    };
    btree_test_case((vptr*)keys2, ARRAY_SIZE(keys2));
    uofs keys3[] = {
        20, 80, 60, 40, 50
    };
    btree_test_case((vptr*)keys3, ARRAY_SIZE(keys3));
    uofs keys4[] = {
        20, 80, 60, 40, 70
    };
    btree_test_case((vptr*)keys4, ARRAY_SIZE(keys4));
    uofs keys5[] = {
        20, 80, 60, 40, 90
    };
    btree_test_case((vptr*)keys5, ARRAY_SIZE(keys5));

    // Test grow two levels.
    uofs keys6[] = {
        20, 80, 60, 40, 10, 30, 70, 90, 15, 85, 32, 35, 62, 65, 38, 61, 36
    };
    btree_test_case((vptr*)keys6, ARRAY_SIZE(keys6));

    test_end();
}
