#include "defs.c"
#include "test.h"

static void defs_base_test()
{
    TEST_ASSERT(sizeof(uofs) == sizeof(vptr));

#ifdef NDEBUG
    // This assert shouldn't break.
    assert(false);
#endif

    // log_print("Timestamp");
    // log_print(" %.3f\n", 0.0f);
    // log_print("Next line\n");
}

static void defs_time_test()
{
    struct timeval start, end, res;
    end.tv_sec = 21;
    end.tv_usec = 600000;
    start.tv_sec = 10;
    start.tv_usec = 500000;
    timer_diff(&end, &start, &res);
    TEST_ASSERT(res.tv_sec == 11 && res.tv_usec == 100000);
    start.tv_sec = 10;
    start.tv_usec = 850000;
    timer_diff(&end, &start, &res);
    TEST_ASSERT(res.tv_sec == 10 && res.tv_usec == 750000);
}

void defs_test()
{
    test_begin("Base");
    defs_base_test();
    defs_time_test();
    test_end();
}
