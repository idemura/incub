#define _GNU_SOURCE
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <search.h>
#include <stdio.h>

#define array_of(a) (sizeof(a) / sizeof(a[0]))

typedef void *rbtree_t;
typedef long long int lli;

int str_cmpf(const void *p1, const void *p2) {
    return strcmp((const char*)p1, (const char*)p2);
}
int int_cmpf(const void *p1, const void *p2) {
    return (long)p1 - (long)p2;
}
void no_free(void *p) {
}

int main(int argc, char **argv)
{
    int t = 0;
    scanf("%d", &n);
    return 0;
}