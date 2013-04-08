#include "defs.c"
#include "test.h"

void defs_test()
{
    test_begin("Defs");

    struct timeval start, end, res;
    end.tv_sec = 21;
    end.tv_usec = 600;

    start.tv_sec = 10;
    start.tv_usec = 500;
    timer_diff(&end, &start, &res);
    TEST_CHECK(res.tv_sec == 11 && res.tv_usec == 100);

    start.tv_sec = 10;
    start.tv_usec = 700;
    timer_diff(&end, &start, &res);
    TEST_CHECK(res.tv_sec == 10 && res.tv_usec == 999800);

    test_end();
}
