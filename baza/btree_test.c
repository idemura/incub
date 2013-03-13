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
        n += btree_count(node->subnode[i].ptr, depth - 1);
    }
    return n + node->num;
}

static bool btree_check_keys(struct btree_node *node, int depth)
{
    bool ok = true;
    for (int i = 1; i < node->num; ++i) {
        if (!(node->subnode[i - 1].key < node->subnode[i].key)) {
            ok = false;
            break;
        }
    }

    if (!ok) {
        fprintf(stderr, "Btree node keys:\n  ");
        for (int i = 0; i < node->num; ++i) {
            fprintf(stderr, "%i ", node->subnode[i].key);
        }
        fprintf(stderr, "\n");
        return false;
    }

    if (depth > 1) {
        for (int i = 0; i <= node->num; ++i) {
            if (!btree_check_keys(node->subnode[i].ptr, depth - 1)) {
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
    if (!btree_check_keys(bt->root, bt->depth)) {
        return false;
    }
    return true;
}

void btree_test()
{
    struct btree *bt;

    test_begin("Btree");

    TEST_CHECK(btree_memory() == 0);
    bt = btree_create();
    TEST_CHECK(btree_memory() != 0);
    btree_destroy(bt);
    TEST_CHECK(btree_memory() == 0);

    int_key key[] = {
        10, 20, 15, 5, 17
    };
    int val[ARRAY_SIZE(key)];
    int update_val[ARRAY_SIZE(key)];
    for (int i = 0; i < ARRAY_SIZE(key); ++i) {
        val[i] = i;
        update_val[i] = 100 + i;
    }

    bt = btree_create();
    TEST_CHECK(btree_size(bt) == 0);
    TEST_CHECK(btree_check(bt));

    for (int i = 0; i < ARRAY_SIZE(key); ++i) {
        btree_insert(bt, key[i], &val[i]);
        TEST_CHECK(btree_size(bt) == i + 1);
        TEST_CHECK(btree_find(bt, key[i]) == &val[i]);
        TEST_CHECK(btree_check(bt));
        if (i == 0) {
            TEST_CHECK(btree_find(bt, key[1]) == NULL);
        }
    }

    for (int i = 0; i < ARRAY_SIZE(key); ++i) {
        btree_insert(bt, key[i], &update_val[i]);
        TEST_CHECK(btree_find(bt, key[i]) == &update_val[i]);
        TEST_CHECK(btree_size(bt) == ARRAY_SIZE(key));
    }

    btree_destroy(bt);
    TEST_CHECK(btree_memory() == 0);

    test_end();
}
