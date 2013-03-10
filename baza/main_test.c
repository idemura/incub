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

    char *val = sdsdupz("value");

    bt = btree_create();
    TEST_CHECK(btree_size(bt) == 0);
    TEST_CHECK(btree_check(bt));
    btree_insert(bt, 10, val);
    TEST_CHECK(btree_size(bt) == 1);
    TEST_CHECK(btree_check(bt));
    btree_destroy(bt);
    TEST_CHECK(btree_memory() == 0);

    sdsfree(val);
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
