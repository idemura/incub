#include "stack.c"
#include "test.h"

void stack_test()
{
    test_begin("Stack");

    struct stack st;
    stack_alloc(&st, 8);
    TEST_CHECK(stack_empty(&st));
    stack_push(&st, 10);
    stack_push(&st, 20);
    TEST_CHECK(stack_pop(&st) == 20);
    TEST_CHECK(stack_pop(&st) == 10);
    TEST_CHECK(stack_empty(&st));
    stack_free(&st);

    const iref n = 1000;
    stack_alloc(&st, n);
    TEST_CHECK(stack_empty(&st));
    for (iref i = 0; i < n; ++i) {
      stack_push(&st, i);
    }
    for (iref i = 0; i < n; ++i) {
      TEST_CHECK(stack_pop(&st) == (n - 1 - i));
    }
    TEST_CHECK(stack_empty(&st));
    stack_free(&st);

    test_end();
}
