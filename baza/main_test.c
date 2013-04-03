#include "test.h"

void dstr_test();
void btree_test();
void pque_test();

int main()
{
    dstr_test();
    // btree_test();
    pque_test();
    return test_report();
}
