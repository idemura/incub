/*
  Copyright 2013 Igor Demura

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
*/
#include "pque.c"
#include "test.h"

static int uint_less(vptr k1, vptr k2)
{
    return (uofs)k1 - (uofs)k2;
}

static bool pque_check_impl(struct pque *pq)
{
    if (!pq) {
        return true;
    }
    for (uofs i = 1; i < pq->size; ++i) {
        uofs p = (i - 1) / 2;
        if (pq->cmpf(pq->heap[p], pq->heap[i]) > 0) {
            fprintf(test_out(), "Heap violation at %zi (parent %zi)\n", i, p);
            return false;
        }
    }
    return true;
}

static void pque_print_test(struct pque *pq)
{
    for (uofs i = 0; i < pq->size; ++i) {
        fprintf(test_out(), "%lu ", (uofs)pq->heap[i]);
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

static void pque_test_sort(uofs keys, int n)
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
    for (uofs i = 0; i < ARRAY_SIZE(keys); ++i) {
        pque_insert(pq, (vptr)keys[i]);
        TEST_CHECK(pque_check(pq));
        TEST_CHECK(pque_size(pq) == i + 1);
    }
    TEST_CHECK(pque_pop(pq) == (vptr)5);
    pque_destroy(pq);

    // Check reallocations
    pq = pque_create(uint_less, 8);
    TEST_CHECK(pq != NULL);
    for (uofs i = 0; i < 12; ++i) {
        vptr key = (vptr)(999 - i);
        pque_insert(pq, key);
        TEST_CHECK(pque_check(pq));
        TEST_CHECK(pque_top(pq) == key);
        TEST_CHECK(pque_size(pq) == i + 1);
    }
    pque_destroy(pq);

    test_end();
}
