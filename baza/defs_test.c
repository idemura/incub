#include "defs.c"
#include "test.h"

static void defs_base_test(void)
{
    TEST_CHECKM(sizeof(uofs) == sizeof(vptr),
        "sizeof(uofs) %zu != sizeof(vptr) %zu\n",
        sizeof(uofs), sizeof(vptr));

#ifdef NDEBUG
    // This assert shouldn't break.
    assert(false);
#endif

    // log_print("Timestamp");
    // log_print(" %.3f\n", 0.0f);
    // log_print("Next line\n");
}

static void defs_time_test(void)
{
    struct timeval start, end, res;
    end.tv_sec = 21;
    end.tv_usec = 600000;
    start.tv_sec = 10;
    start.tv_usec = 500000;
    timer_diff(&end, &start, &res);
    TEST_CHECKM(res.tv_sec == 11 && res.tv_usec == 100000,
        "Time diff 1 sec=%li usec=%li\n",
        res.tv_sec, res.tv_usec);
    start.tv_sec = 10;
    start.tv_usec = 850000;
    timer_diff(&end, &start, &res);
    TEST_CHECKM(res.tv_sec == 10 && res.tv_usec == 750000,
        "Time diff 2 sec=%li usec=%li\n",
        res.tv_sec, res.tv_usec);
}

void defs_test(void)
{
    test_begin("Base");
    defs_base_test();
    defs_time_test();
    test_end();
}
