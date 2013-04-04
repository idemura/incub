#include "stack.c"
#include "test.h"

void stack_test()
{
    test_begin("Stack");

    struct stack st;
    stack_alloc(&st, 8);
    TEST_CHECK(stack_empty(&st));
    stack_pushi(&st, 10);
    TEST_CHECK(!stack_empty(&st));
    stack_pushi(&st, 20);
    TEST_CHECK(!stack_empty(&st));
    TEST_CHECK(stack_popi(&st) == 20);
    TEST_CHECK(stack_popi(&st) == 10);
    TEST_CHECK(stack_empty(&st));
    stack_free(&st);

    const uofs n = 1000;
    stack_alloc(&st, n);
    TEST_CHECK(stack_empty(&st));
    for (uofs i = 0; i < n; ++i) {
      stack_pushi(&st, i);
    }
    for (uofs i = 0; i < n; ++i) {
      TEST_CHECK(stack_popi(&st) == (n - 1 - i));
    }
    TEST_CHECK(stack_empty(&st));
    stack_free(&st);

    test_end();
}
