#define _GNU_SOURCE
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <search.h>
#include <stdio.h>

#define array_of(a) (sizeof(a) / sizeof(a[0]))

typedef void *rbtree_t;

int str_cmpf(const void *p1, const void *p2) {
    return strcmp((const char*)p1, (const char*)p2);
}
int int_cmpf(const void *p1, const void *p2) {
    return (long)p1 - (long)p2;
}
void nothing(void *p) {
}


int main(int argc, char **argv)
{
    int n = 0, i;
    int nmax;
    int *a = 0;
    int count[1000];
    int yes = 1;

    scanf("%d", &n);
    a = malloc(sizeof(int) * n);
    for (i = 0; i < n; ++i) {
        scanf(" %d", &a[i]);
    }

    nmax = (n + 1) / 2;
    memset(count, 0, sizeof(count));
    for (i = 0; i < n; ++i) {
        int q = ++count[a[i] - 1];
        if (q > nmax) {
            yes = 0;
            break;
        }
    }

    int op = 10;

    free(a);
    printf(yes? "YES\n": "NO\n");
    return 0;
}
