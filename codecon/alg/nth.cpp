#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <memory.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))

typedef long long int lli;

void swap(lli* p1, lli* p2)
{
    lli t;
    t = *p1;
    *p1 = *p2;
    *p2 = t;
}

void print(lli* a, int a_n, int i, int j)
{
    int k;
    for (k = 0; k < a_n; k++) {
        printf("%lld ", a[k]);
    }
    printf("\n");
    if (i >= 0 && j >= 0) {
        for (k = 0; k < a_n; k++) {
            if (k == i) {
                printf("i ");
            } else if (k == j) {
                printf("j ");
            } else {
                printf("  ");
            }
        }
        printf("\n");
    }
}

int dd;
lli nth_rec(lli* a, int a_n, int i0, int j0, int ith)
{
    printf("search in %d-%d\n", i0, j0);
    print(a, a_n, i0, j0);
    dd++;
    if (dd == 10) {
        printf("AAAAAAAAA!!!!!\n");
        exit(0);
    }
    int i, j;

    if (i0 == j0) {
        printf("found ith = 0\n");
        return a[i0];
    }

    i = i0;
    j = j0;
    lli d = a[0];
    printf("split item: %lld\n", d);
    do {
        for (; a[i] < d; i++) {
        }
        for (; j >= i && a[j] >= d; j--) {
        }
        print(a, a_n, i, j);
        // 'i' can't be equal to 'j', because can't be at the same time
        // a[i] >= d(from the first loop) and a[i] < d (from the second
        // loop break condition).
        if (i < j) {
            lli t = a[i];
            a[i] = a[j];
            a[j] = t;
            i++;
            j--;
            printf("after swap:\n");
            print(a, a_n, i, j);
        }
    } while (i <= j);
    printf("break cycle i=%d j=%d\n", i, j);
    // Here 'i' is count of a[i] such that a[i] < d.
    if (ith < i) {
        printf("branch 1\n");
        return nth_rec(a, a_n, i0, j, ith);
    } else {
        printf("branch 0\n");
        return nth_rec(a, a_n, i, j0, ith);
    }
}

lli nth(lli* a, int a_n, int ith)
{
    assert(ith >= 0 && ith < a_n);
    printf("search for %d-th\n", ith);
    return nth_rec(a, a_n, 0, a_n - 1, ith);
}

int main()
{
    int i;
    lli a_src[] = {5, 8, 3, 7, 2, 4, 6, 9, 1, 0};
    lli a[ARRAY_SIZEOF(a_src)];
    const int a_n = ARRAY_SIZEOF(a);

    for (i = 0; i < a_n; i++) {
        memcpy(a, a_src, sizeof a_src);
        // assert(nth(a, a_n, i) == i);
        printf("nth: %lld\n", nth(a, a_n, i));
        break;
    }

    return 0;
}
