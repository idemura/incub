#include "btree.c"
#include "test.h"

static FILE* btree_out;

static void *voidp(void *p)
{
    return p;
}

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

static bool btree_check_keys(struct btree_node *node,
        int depth,
        int min_keys, int max_keys,
        key_t max_key_value)
{
    if (!node) {
        return true;
    }
    if (!btree_check_keys_num(node, min_keys, max_keys)) {
        fprintf(btree_out, "Node %p num %i, must be %d-%d\n",
                voidp(node), node->num, min_keys, max_keys);
        return false;
    }

    int i;
    for (i = 1; i < node->num; ++i) {
        if (!(node->branch[i - 1].key < node->branch[i].key)) {
            fprintf(btree_out, "Node %p key order: %li >= %li @%i,%i\n",
                    voidp(node), node->branch[i - 1].key, node->branch[i].key,
                    i - 1, i);
            break;
        }
    }

    for (i = 0; i < node->num; ++i) {
        if (!(node->branch[i].key < max_key_value)) {
            fprintf(btree_out, "Node %p key bound: %li >= %li @%i\n",
                    voidp(node), node->branch[i].key, max_key_value, i);
            break;
        }
    }

    if (depth > 1) {
        for (i = 0; i < node->num; ++i) {
            if (!btree_check_keys(node->branch[i].ptr, depth - 1,
                    min_keys, max_keys, node->branch[i].key)) {
                return false;
            }
        }
        if (!btree_check_keys(node->branch[node->num].ptr, depth - 1,
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
        fprintf(btree_out, "Node %p parent %p != %p\n",
                voidp(node), voidp(node->parent), voidp(parent));
        return false;
    }

    if (depth > 1) {
        for (int i = 0; i <= node->num; ++i) {
            if (!btree_check_parent(node->branch[i].ptr, depth - 1, node)) {
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
        fprintf(btree_out, "Btree size %zu != %zu\n", bt->size, size);
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

static void btree_print_nodes(struct btree_node *node, int depth)
{
    if (!node) {
        return;
    }

    fprintf(btree_out, "%p parent=%p num=%i depth=%i\n",
            voidp(node->parent), voidp(node), node->num, depth);
    if (node->num == 0) {
        return;
    }
    fprintf(btree_out, "   ");
    for (int i = 0; i < node->num; ++i) {
        fprintf(btree_out, "%p %li ", voidp(node->branch[i].ptr),
                node->branch[i].key);
    }
    fprintf(btree_out, "%p\n", voidp(node->branch[node->num].ptr));
    if (depth > 0) {
        for (int i = 0; i <= node->num; ++i) {
            btree_print_nodes(node->branch[node->num].ptr, depth - 1);
        }
    }
}

static bool btree_check_print(struct btree *bt)
{
    bool ret = btree_check(bt);
    if (!ret) {
        btree_print_nodes(bt->root, bt->depth);
    }
    return ret;
}

void btree_test_insert(key_t *keys, int keys_num)
{
    struct btree *bt = btree_create(2);
    for (int i = 0; i < keys_num; ++i) {
        // fprintf(btree_out, "Insert %li\n", keys[i]);
        btree_insert(bt, keys[i], &keys[i]);
        TEST_CHECK(btree_check_print(bt));
        TEST_CHECK(btree_size(bt) == i + 1);
        for (int j = 0; j < i; ++j) {
            void *val = btree_find(bt, keys[j]);
            if (val != &keys[j]) {
                fprintf(btree_out, "Key %li not found\n", keys[j]);
            }
            TEST_CHECK(val == &keys[j]);
        }
    }

    key_t *new_val = malloc(keys_num * sizeof(key_t));
    memset(new_val, 0, keys_num * sizeof(key_t));
    for (int i = 0; i < keys_num; ++i) {
        // fprintf(btree_out, "Update %li\n", keys[i]);
        btree_insert(bt, keys[i], &new_val[i]);
        TEST_CHECK(btree_find(bt, keys[i]) == &new_val[i]);
        TEST_CHECK(btree_size(bt) == keys_num);
    }
    free(new_val);

    btree_destroy(bt);
    TEST_CHECK(btree_memory == 0);
}

void btree_test()
{
    struct btree *bt;

    test_begin("Btree");
    btree_out = stderr;

    TEST_CHECK(btree_memory == 0);
    bt = btree_create(2);
    TEST_CHECK(btree_memory != 0);
    btree_destroy(bt);
    TEST_CHECK(btree_memory == 0);

    key_t keys1[] = {
        10, 20, 15, 5
    };
    btree_test_insert(keys1, ARRAY_SIZE(keys1));
    key_t keys2[] = {
        10, 20, 15, 5, 3
    };
    btree_test_insert(keys2, ARRAY_SIZE(keys2));
    key_t keys3[] = {
        10, 20, 15, 5, 7
    };
    btree_test_insert(keys3, ARRAY_SIZE(keys3));
    key_t keys4[] = {
        10, 20, 15, 5, 13
    };
    btree_test_insert(keys4, ARRAY_SIZE(keys4));
    key_t keys5[] = {
        10, 20, 15, 5, 17
    };
    btree_test_insert(keys5, ARRAY_SIZE(keys5));
    key_t keys6[] = {
        10, 20, 15, 5, 23
    };
    btree_test_insert(keys6, ARRAY_SIZE(keys6));

    test_end();
}
