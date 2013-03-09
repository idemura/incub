#include "test.h"

#include <stdio.h>

static const char *s_name;
static int s_failed_asserts;
static int s_failed;
static int s_passed;

void test_begin(const char *name)
{
    s_name = name;
    s_failed_asserts = 0;
}

void test_end()
{
    if (s_failed_asserts == 0) {
        s_passed++;
        printf("%s: passed\n", s_name);
    } else {
        s_failed++;
        printf("%s: FAILED\n", s_name);
    }
}

void test_check(int ok, const char *expr, const char *file, int line)
{
    if (!ok) {
        printf("FAIL %s@%d: %s\n", file, line, expr);
        s_failed_asserts++;
    }
}

int test_report()
{
    if (s_failed == 0) {
        printf("%d passed\n", s_passed);
    } else {
        printf("%d passed, %d FAILED!\n", s_passed, s_failed);
    }
    return s_failed == 0? 0: -1;
}
