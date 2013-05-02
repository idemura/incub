#define _GNU_SOURCE
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <search.h>
#include <stdio.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define ZERO(p, n)      memset(p, 0, n);

#define MOD 1000000007

typedef void *rbtree_t;
typedef long long int lli;

int mini(int a, int b) {
    return a < b? a: b;
}
int maxi(int a, int b) {
    return a > b? a: b;
}

int main(int argc, char **argv)
{
    int a = 0, b = 0, n = 0;

    scanf("%d%d", &a, &b, &n);
    /* Find sums of good numbers: a*k1 + b*k2, where k1 + k2 == n or using
       one variable: a * j + b * (n - j). Now we want find which of sums are
       good. We can do like binary search starting with min and max and looking
       for ranges where major digit is a or b
    */
    return 0;
}
