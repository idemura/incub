#define _GNU_SOURCE
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <search.h>
#include <stdio.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))

typedef void *rbtree_t;
typedef long long int lli;

int mini(int a, int b) {
    return a < b? a: b;
}
int maxi(int a, int b) {
    return a > b? a: b;
}
void no_free(void *p) {
}

typedef struct {
    int type;
    int p1, p2;
} op_t;

int main(int argc, char **argv)
{
    int N = 0;
    scanf("%d", &N);
    int* xs = malloc(N * sizeof(*xs));
    for (int i = 0; i < N; ++i) {
    }
    return 0;
}
