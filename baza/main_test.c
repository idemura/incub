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
#include "test.h"
#include "defs.h"

void dstr_test();
void stack_test();
void pque_test();
void btree_test();

void base_test()
{
    test_begin("Base");
    TEST_CHECK(sizeof(uofs) == sizeof(vptr));
#ifdef NDEBUG
    // This assert shouldn't break.
    assert(false);
#endif
    test_end();
}

int main()
{
    base_test();
    dstr_test();
    stack_test();
    pque_test();
    btree_test();
    return test_report();
}
