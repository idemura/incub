#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <memory.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))

typedef long long int lli;

void swap(lli* p1, lli* p2)
{
    lli tmp = *p1;
    *p1 = *p2;
    *p2 = tmp;
}

void print(lli* a, int a_n, int i, int j)
{
    int k;
    for (k = 0; k < a_n; k++) {
        printf("%lld ", a[k]);
    }
    printf("\n");
    for (k = 0; k < a_n; k++) {
        if (k == i && i == j) {
            printf("i,j");
        } else if (k == i) {
            printf("i ");
        } else if (k == j) {
            printf("j ");
        } else {
            printf("  ");
        }
    }
    printf("\n");
}

lli nth_rec(lli* a, int a_n, int i0, int j0, int ith)
{
    int i, j;

    if (i0 == j0) {
        return a[i0];
    }

    i = i0; // 'i' is count of items less than 'd'.
    j = j0;
    lli d = a[i];
    for (;;) {
        for (; a[i] < d; i++) {
        }
        for (; j > i && a[j] >= d; j--) {
        }
        // 'i' can't be equal to 'j', because can't be at the same time
        // a[i] >= d(from the first loop) and a[i] < d (from the second
        // loop break condition).
        if (i < j) {
            swap(&a[i], &a[j]);
            i++;
            j--;
        } else {
            break;
        }
    }

    // Here 'i' is count of a[i] such that a[i] < d.
    if (i == i0) {
        if (i == ith) {
            return a[i];
        } else {
            i++;
        }
    }

    if (ith < i) {
        return nth_rec(a, a_n, i0, i - 1, ith);
    } else {
        return nth_rec(a, a_n, i, j0, ith);
    }
}

lli nth(lli* a, int a_n, int ith)
{
    assert(ith >= 0 && ith < a_n);
    return nth_rec(a, a_n, 0, a_n - 1, ith);
}

void test1()
{
    int i;
    lli a_src[] = {5, 8, 3, 7, 2, 4, 6, 9, 1, 0};
    lli a[ARRAY_SIZEOF(a_src)];
    const int a_n = ARRAY_SIZEOF(a);

    for (i = 0; i < a_n; i++) {
        memcpy(a, a_src, sizeof a_src);
        assert(nth(a, a_n, i) == i);
    }

    printf("Test 1 OK\n");
}

void test2()
{
    int i;
    lli a_src[] = {5, 5, 5, 5, 5};
    lli a[ARRAY_SIZEOF(a_src)];
    const int a_n = ARRAY_SIZEOF(a);

    for (i = 0; i < a_n; i++) {
        memcpy(a, a_src, sizeof a_src);
        assert(nth(a, a_n, i) == 5);
    }

    printf("Test 2 OK\n");
}

void test3()
{
    int i;
    lli a_src[] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
    lli a[ARRAY_SIZEOF(a_src)];
    const int a_n = ARRAY_SIZEOF(a);

    for (i = 0; i < a_n; i++) {
        memcpy(a, a_src, sizeof a_src);
        assert(nth(a, a_n, i) == i);
    }

    printf("Test 3 OK\n");
}

int main()
{
    test1();
    test2();
    test3();
    return 0;
}
