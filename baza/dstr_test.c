#include "dstr.c"
#include "test.h"

void dstr_test()
{
    test_begin("DStr");

    char* s1 = dstr_dup("1234");
    TEST_CHECK(strcmp(s1, "1234") == 0);
    TEST_CHECK(dstr_len(s1) == 4);
    dstr_free(s1);

    char* s2 = dstr_new(0);
    TEST_CHECK(strcmp(s2, "") == 0);
    TEST_CHECK(dstr_len(s2) == 0);
    dstr_free(s2);

    test_end();
}
