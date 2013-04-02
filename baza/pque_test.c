#include "pque.c"
#include "test.h"

#include <stdio.h>

static bool uint_less(pque_key k1, pque_key k2)
{
    return (uintptr_t)k1 < (uintptr_t)k2;
}

static bool pque_check(struct pque *pq)
{
    return true;
}

void pque_test()
{
    struct pque *pq = NULL;
    test_begin("PQue");

    // Destroy NULL is OK
    pque_destroy(NULL);

    pq = pque_create(uint_less, 0);
    TEST_CHECK(pq != NULL);
    TEST_CHECK(pque_size(pq) == 0);
    pque_destroy(pq);

    // pq = pque_create(uint_less, 0);
    // TEST_CHECK(pq != NULL);
    // uintptr_t keys[] = {
    //     10, 20, 30, 5, 15
    // };
    // for (int i = 0; i < ARRAY_SIZE(keys); ++i) {
    //     pque_insert(pq, (pque_key)keys[i]);
    //     TEST_CHECK(pque_check(pq));
    //     TEST_CHECK(pque_size(pq) == i + 1);
    // }
    // pque_destroy(pq);

    test_end();
}
