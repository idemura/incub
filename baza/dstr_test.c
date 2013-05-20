#include "dstr.c"
#include "test.h"

void dstr_test(void)
{
    test_begin("DStr");

    char* s1 = dstr_dup("1234");
    TEST_CHECKM(strcmp(s1, "1234") == 0,
        "dstr_dup: wrong string");
    TEST_CHECKM(dstr_len(s1) == 4,
        "dstr_dup: wrong length");
    dstr_free(s1);

    char* s2 = dstr_new(0);
    TEST_CHECKM(strcmp(s2, "") == 0,
        "dst_new: string != \"\"");
    TEST_CHECKM(dstr_len(s2) == 0,
        "dst_new: length != 0");
    dstr_free(s2);

    char* s3 = dstr_dup("1234");
    TEST_CHECKM(strcmp(s3, "1234") == 0,
        "dstr_dup: wrong string");
    dstr_clear(s3);
    TEST_CHECKM(strcmp(s3, "") == 0,
        "dstr_clear: string != \"\"");
    TEST_CHECKM(dstr_len(s3) == 0,
        "dstr_clear: length != 0");
    dstr_free(s3);

    test_end();
}
