#include "dstr.c"
#include "test.h"

void dstr_test()
{
    test_begin("DStr");

    char* s1 = dstr_dup("1234");
    TEST_ASSERT(strcmp(s1, "1234") == 0);
    TEST_ASSERT(dstr_len(s1) == 4);
    dstr_free(s1);

    char* s2 = dstr_new(0);
    TEST_ASSERT(strcmp(s2, "") == 0);
    TEST_ASSERT(dstr_len(s2) == 0);
    dstr_free(s2);

    char* s3 = dstr_dup("1234");
    TEST_ASSERT(strcmp(s3, "1234") == 0);
    dstr_clear(s3);
    TEST_ASSERT(strcmp(s3, "") == 0);
    TEST_ASSERT(dstr_len(s3) == 0);
    dstr_free(s3);

    test_end();
}
