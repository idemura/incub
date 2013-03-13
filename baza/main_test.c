#include "test.h"

void sds_test();
void btree_test();

int main()
{
    sds_test();
    btree_test();
    return test_report();
}
