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

static int uint_cmp(vptr k1, vptr k2)
{
    return (uofs)k1 - (uofs)k2;
}

static bool pque_check(struct pque *pq)
{
    if (!pq) {
        return true;
    }
    uofs i;
    for (i = 1; i < pq->size; ++i) {
        uofs p = (i - 1) / 2;
        TEST_CHECKR(pq->cmpf(pq->heap[p], pq->heap[i]) <= 0,
            "Heap violation at %zi (parent %zi)\n",
            i, p);
    }
    return true;
}

static void pque_print(struct pque *pq)
{
    uofs i;
    for (i = 0; i < pq->size; ++i) {
        fprintf(test_out(), "%zu ", (uofs)pq->heap[i]);
    }
    fprintf(test_out(), "\n");
}

static bool pque_check_print(struct pque *pq)
{
    if (pque_check(pq)) {
        return true;
    }
    pque_print(pq);
    return false;
}

static void pque_test_sort(uofs keys, int n)
{
}

void pque_test()
{
    uofs i;
    struct pque *pq = NULL;
    test_begin("PQue");

    // Destroy NULL is OK
    pque_destroy(NULL);

    pq = pque_create(uint_cmp, 1);
    TEST_CHECKM(pq != NULL,
        "pque_create NULL");
    TEST_CHECKM(pque_size(pq) == 0,
        "pque empty size %zu",
        pque_size(pq));
    TEST_CHECKM(pque_top(pq) == NULL,
        "pque empty top != NULL");
    pque_destroy(pq);

    pq = pque_create(uint_cmp, 8);
    TEST_CHECKM(pq != NULL,
        "pque_create NULL");

    uofs keys[] = {
        10, 20, 30, 5, 15
    };
    uofs keys_sorted[] = {
        5, 10, 15, 20, 30
    };
    for (i = 0; i < ARRAY_SIZEOF(keys); ++i) {
        pque_insert(pq, (vptr)keys[i]);
        pque_check_print(pq);
        TEST_CHECKM(pque_size(pq) == i + 1,
            "pque_size %zu (%zu expected)",
            pque_size(pq), i + 1);
    }
    for (i = 0; i < ARRAY_SIZEOF(keys); ++i) {
        uofs key_pop = (uofs)pque_pop(pq);
        TEST_CHECKM(key_pop == keys_sorted[i],
            "pque_pop %zu (%zu expected)",
            key_pop, keys_sorted[i]);
    }
    pque_destroy(pq);

    // Check reallocations
    const uofs n = 10;
    pq = pque_create(uint_cmp, 8);
    TEST_CHECK(pq != NULL);
    for (i = 0; i < n; ++i) {
        vptr key = (vptr)(199 - i);
        pque_insert(pq, key);
        pque_check_print(pq);
        TEST_CHECKM(pque_top(pq) == key,
            "pque_top %zu (%zu expected)",
            (uofs)pque_top(pq), (uofs)key);
        TEST_CHECKM(pque_size(pq) == i + 1,
            "pque_size size %zu (%zu expected)",
            pque_size(pq), i + 1);
    }
    for (i = 0; i < n; ++i) {
        vptr key = (vptr)(200 - n + i);
        vptr key_pop = pque_pop(pq);
        TEST_CHECKM(key_pop == key,
            "pque_pop %zu (%zu expected)",
            key_pop, key);
        pque_check_print(pq);
    }
    pque_destroy(pq);

    test_end();
}
