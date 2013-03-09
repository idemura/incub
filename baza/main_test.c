#include "test.h"
#include "sds.h"

#include <string.h>

void string_test()
{
    TEST("sds string");

    char* s1 = sdsdup("1234");
    EXPECT(strcmp(s1, "1234") == 0);
    EXPECT(sdslen(s1) == 4);

    char* s2 = sdsempty(0);
    EXPECT(strcmp(s2, "") == 0);
    EXPECT(sdslen(s2) == 0);

    TEST_END();
}

int main()
{
    string_test();
    return test_report();
}
