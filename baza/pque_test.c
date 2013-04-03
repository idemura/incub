#include "pque.c"
#include "test.h"

static bool uint_less(pque_key k1, pque_key k2)
{
    return (uintptr_t)k1 < (uintptr_t)k2;
}

static bool pque_check_impl(struct pque *pq)
{
    if (!pq) {
        return true;
    }
    for (iref i = 1; i < pq->size; ++i) {
        iref p = (i - 1) / 2;
        if (!pq->cmpf(pq->heap[p], pq->heap[i])) {
            fprintf(test_out(), "Heap violation at %zi (parent %zi)\n", i, p);
            return false;
        }
    }
    return true;
}

static void pque_print_test(struct pque *pq)
{
    for (iref i = 0; i < pq->size; ++i) {
        fprintf(test_out(), "%lu ", (uintptr_t)pq->heap[i]);
    }
    fprintf(test_out(), "\n");
}

static bool pque_check(struct pque *pq)
{
    if (!pque_check_impl(pq)) {
        pque_print_test(pq);
        return false;
    }
    return true;
}

static void pque_test_sort(uintptr_t keys, int n)
{
    for (int i = 0; i < n; ++i) {
    }
}

void pque_test()
{
    struct pque *pq = NULL;
    test_begin("PQue");

    // Destroy NULL is OK
    pque_destroy(NULL);

    pq = pque_create(uint_less, 1);
    TEST_CHECK(pq != NULL);
    TEST_CHECK(pque_size(pq) == 0);
    TEST_CHECK(pque_top(pq) == NULL);
    pque_destroy(pq);

    pq = pque_create(uint_less, 8);
    TEST_CHECK(pq != NULL);
    uintptr_t keys[] = {
        10, 20, 30, 5, 15
    };
    for (iref i = 0; i < ARRAY_SIZE(keys); ++i) {
        pque_insert(pq, (pque_key)keys[i]);
        TEST_CHECK(pque_check(pq));
        TEST_CHECK(pque_size(pq) == i + 1);
    }
    TEST_CHECK(pque_pop(pq) == (pque_key)5);
    pque_destroy(pq);

    // Check reallocations
    pq = pque_create(uint_less, 8);
    TEST_CHECK(pq != NULL);
    for (iref i = 0; i < 12; ++i) {
        pque_key key = (pque_key)(999 - i);
        pque_insert(pq, key);
        TEST_CHECK(pque_check(pq));
        TEST_CHECK(pque_top(pq) == key);
        TEST_CHECK(pque_size(pq) == i + 1);
    }
    pque_destroy(pq);

    test_end();
}
