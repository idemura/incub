#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <memory.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))

typedef long long int lli;

void swap(lli *p1, lli *p2)
{
    lli tmp = *p1;
    *p1 = *p2;
    *p2 = tmp;
}

// 'a' items are 0, 1, ...
void counting_sort(lli *a, int a_n)
{
    int i, j, k;

    if (a_n == 0) {
        return;
    }

    j = 0;
    for (i = 1; i < a_n; i++) {
        if (a[i] > a[j]) {
            j = i;
        }
    }

    lli max_a = a[j];
    int *c = new int[max_a + 1]();
    for (i = 0; i < a_n; i++) {
        c[a[i]]++;
    }
    for (i = 1; i <= max_a; i++) {
        c[i] += c[i-1];
    }
    k = 0;
    for (i = 0; i <= max_a; i++) {
        for (j = k; j < c[i]; j++) {
            a[j] = i;
        }
        // CLR recommends to put value backwards to make stable:
        // for (j = c[i]; j > k; j--) {
        //     a[j-1] = i;
        // }
        k = c[i];
    }
}

bool sorted(lli *a, int a_n)
{
    int i;
    for (i = 1; i < a_n; i++) {
        if (a[i-1] > a[i]) {
            return false;
        }
    }
    return true;
}

void test1()
{
    int i;
    lli a_src1[] = {5, 8, 3, 7, 2, 4, 6, 9, 1, 0};
    lli a_src2[] = {5, 5, 5, 7, 2, 4, 6, 6, 1, 0};
    lli a[ARRAY_SIZEOF(a_src1)];
    const int a_n = ARRAY_SIZEOF(a);

    memcpy(a, a_src1, sizeof a_src1);
    counting_sort(a, a_n);
    assert(sorted(a, a_n));

    memcpy(a, a_src2, sizeof a_src2);
    counting_sort(a, a_n);
    assert(sorted(a, a_n));

    printf("Test 1 OK\n");
}

int main()
{
    test1();
    return 0;
}

