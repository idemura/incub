#ifndef TEST_H_
#define TEST_H_

#define TEST(name) \
    do { \
        test_begin(name); \
    } while (0)

#define TEST_END() \
    do { \
        test_end(); \
    } while (0)

#define EXPECT(expr) \
    do { \
        test_check(expr, #expr, __FILE__, __LINE__); \
    } while (0)

void test_begin(const char *name);
void test_end();
void test_check(int ok, const char *expr, const char *file, int line);
int test_report();

#endif
