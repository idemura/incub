#include "test.h"
#include "sds.h"
#include "btree.h"
#include <string.h>
#include <stdio.h>

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
        10, 20, 15
    };
    int val[] = {
        50, 51, 52
    };
    int update_val[ARRAY_SIZE(key)];
    memset(update_val, 0, sizeof(update_val));

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

void string_test()
{
    test_begin("SDS string");

    char* s1 = sdsdupz("1234");
    TEST_CHECK(strcmp(s1, "1234") == 0);
    TEST_CHECK(sdslen(s1) == 4);
    sdsfree(s1);

    char* s2 = sdsempty(0);
    TEST_CHECK(strcmp(s2, "") == 0);
    TEST_CHECK(sdslen(s2) == 0);
    sdsfree(s2);

    test_end();
}

int main()
{
    string_test();
    btree_test();
    return test_report();
}
