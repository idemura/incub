#include "defs.h"
#include <stdio.h>

int main()
{
    printf("%c[%d;%dmColor Me!%c[%dm\n",27,1,33,27,0);
    printf("main\n");
    return 0;
}
