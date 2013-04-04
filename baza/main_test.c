#include "test.h"
#include "defs.h"

void dstr_test();
void stack_test();
void pque_test();
void btree_test();

void base_test()
{
    test_begin("Base");
    TEST_CHECK(sizeof(iref) == sizeof(void*));
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
