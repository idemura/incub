#include "btree.c"
#include "test.h"

static size_t btree_count(struct btree_node *node, int depth)
{
    assert(node);
    if (depth == 0) {
        return node->num;
    }
    size_t n = 0;
    for (int i = 0; i <= node->num; ++i) {
        n += btree_count(node->branch[i].ptr, depth - 1);
    }
    return n + node->num;
}

static bool btree_check_keys_num(struct btree_node *node,
        int min_keys, int max_keys)
{
    return !node->parent || (min_keys <= node->num && node->num <= max_keys);
}

static bool btree_check_keys(struct btree_node *node, int depth,
        int min_keys, int max_keys)
{
    if (!node) {
        return true;
    }
    if (!btree_check_keys_num(node, min_keys, max_keys)) {
        fprintf(stderr, "Btree node: %i keys, must be %d-%d\n", node->num,
                min_keys, max_keys);
        return false;
    }

    bool ok = true;
    for (int i = 1; i < node->num; ++i) {
        if (!(node->branch[i - 1].key < node->branch[i].key)) {
            ok = false;
            break;
        }
    }

    if (!ok) {
        fprintf(stderr, "Btree node keys:\n  ");
        for (int i = 0; i < node->num; ++i) {
            fprintf(stderr, "%li ", node->branch[i].key);
        }
        fprintf(stderr, "\n");
        return false;
    }

    if (depth > 1) {
        for (int i = 0; i <= node->num; ++i) {
            if (!btree_check_keys(node->branch[i].ptr, depth - 1,
                    min_keys, max_keys)) {
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

    size_t size = btree_count(bt->root, bt->depth);
    if (size != bt->size) {
        fprintf(stderr, "Btree size: %zu != %zu\n", bt->size, size);
        return false;
    }
    if (!btree_check_keys(bt->root, bt->depth, bt->min_keys, bt->max_keys)) {
        return false;
    }
    return true;
}

void btree_test()
{
    struct btree *bt;

    test_begin("Btree");

    TEST_CHECK(btree_memory == 0);
    bt = btree_create(2);
    TEST_CHECK(btree_memory != 0);
    btree_destroy(bt);
    TEST_CHECK(btree_memory == 0);

    const key_t keys[] = {
        10, 20 //, 15
    };
    int val[ARRAY_SIZE(keys)];
    int update_val[ARRAY_SIZE(keys)];
    for (int i = 0; i < ARRAY_SIZE(keys); ++i) {
        val[i] = i;
        update_val[i] = 100 + i;
    }

    bt = btree_create(2);
    TEST_CHECK(btree_size(bt) == 0);
    TEST_CHECK(btree_check(bt));

    for (int i = 0; i < ARRAY_SIZE(keys); ++i) {
        printf("---> insert %d key %ld...\n", i + 1, keys[i]);
        btree_insert(bt, keys[i], &val[i]);
        TEST_CHECK(btree_size(bt) == i + 1);
        TEST_CHECK(btree_find(bt, keys[i]) == &val[i]);
        TEST_CHECK(btree_check(bt));
        if (i == 0) {
            TEST_CHECK(btree_find(bt, 20) == NULL);
        }
    }

    for (int i = 0; i < ARRAY_SIZE(keys); ++i) {
        btree_insert(bt, keys[i], &update_val[i]);
        TEST_CHECK(btree_find(bt, keys[i]) == &update_val[i]);
        TEST_CHECK(btree_size(bt) == ARRAY_SIZE(keys));
    }

    btree_destroy(bt);
    TEST_CHECK(btree_memory == 0);

    test_end();
}
