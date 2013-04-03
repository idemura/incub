#ifndef TEST_H_
#define TEST_H_

#include <stdio.h>

#define TEST_CHECK(expr) \
    do { \
        test_check(expr, #expr, __FILE__, __LINE__); \
    } while (0)

void test_init();
void test_begin(const char *name);
void test_end();
void test_check(int ok, const char *expr, const char *file, int line);
int test_report();
FILE *test_out();
void set_test_out(FILE *f);

#endif
