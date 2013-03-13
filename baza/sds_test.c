#include "sds.c"
#include "test.h"

void sds_test()
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
