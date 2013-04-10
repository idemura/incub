#ifndef TEST_H_
#define TEST_H_

#include "defs.h"
#include <stdio.h>

#define TEST_CHECK(expr) \
    do { \
        test_check(expr, #expr, __FILE__, __LINE__, NULL); \
    } while (0)

#define TEST_CHECKM(expr, ...) \
    do { \
        test_check(expr, #expr, __FILE__, __LINE__, __VA_ARGS__); \
    } while (0)

#define TEST_CHECKR(expr, ...) \
    do { \
        if (!test_check(expr, #expr, __FILE__, __LINE__, __VA_ARGS__)) \
            return false; \
    } while (0)

void test_init();
void test_begin(const char *name);
void test_end();
bool test_check(int ok, const char *expr, const char *file, int line,
        const char *format, ...);
void test_report();
int test_failed_count();
int test_passed_count();
FILE *test_out();
void  test_setout(FILE *f);

#endif
