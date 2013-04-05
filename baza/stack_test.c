/*
  Copyright 2013 Igor Demura

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
*/
#include "stack.c"
#include "test.h"

void stack_test()
{
    test_begin("Stack");

    struct stack st;
    stack_alloc(&st);
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
    stack_alloc(&st);
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
