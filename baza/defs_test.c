#include "defs.c"
#include "test.h"

static void defs_time_test()
{
    struct timeval start, end, res;
    end.tv_sec = 21;
    end.tv_usec = 600000;
    start.tv_sec = 10;
    start.tv_usec = 500000;
    timer_diff(&end, &start, &res);
    TEST_CHECK(res.tv_sec == 11 && res.tv_usec == 100000);
    start.tv_sec = 10;
    start.tv_usec = 850000;
    timer_diff(&end, &start, &res);
    TEST_CHECK(res.tv_sec == 10 && res.tv_usec == 750000);
}

void defs_test()
{
    test_begin("Defs");
    defs_time_test();
    test_end();
}
