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
#include "pque.h"

struct pque {
    vptr *heap;
    compare_fn cmpf;
    uofs size;
    uofs capacity;
};

struct pque *pque_create(compare_fn cmpf, uofs capacity)
{
    assert(cmpf);
    if (!cmpf) {
        return NULL;
    }
    struct pque *pq = mem_alloc(sizeof(struct pque));
    if (!pq) {
        return NULL;
    }
    if (capacity < 1) {
        capacity = 1;
    }
    pq->heap = mem_alloc(capacity * sizeof(vptr));
    if (!pq->heap) {
        mem_free(pq);
        return NULL;
    }
    pq->cmpf = cmpf;
    pq->size = 0;
    pq->capacity = capacity;
    return pq;
}

void pque_destroy(struct pque *pq)
{
    if (pq) {
        mem_free(pq->heap);
        mem_free(pq);
    }
}

uofs pque_size(struct pque *pq)
{
    return pq? pq->size: 0;
}

static void pque_heapify(struct pque *pq, uofs i)
{
    while (i != 0) {
        uofs p = (i - 1) / 2;
        if (pq->cmpf(pq->heap[p], pq->heap[i]) > 0) {
            vptr temp = pq->heap[p];
            pq->heap[p] = pq->heap[i];
            pq->heap[i] = temp;
        }
        i = p;
    }
}

void pque_insert(struct pque *pq, vptr key)
{
    if (pq->size == pq->capacity) {
        uofs capacity = pq->capacity + pq->capacity / 2;
        if (capacity <= pq->capacity) {
            capacity += pq->capacity + 1;
        }
        vptr new = mem_alloc(capacity * sizeof(vptr));
        if (!new) {
            return;
        }
        memcpy(new, pq->heap, pq->size * sizeof(vptr));
        mem_free(pq->heap);
        pq->capacity = capacity;
        pq->heap = new;
    }

    pq->heap[pq->size] = key;
    pque_heapify(pq, pq->size);
    pq->size++;
}

vptr pque_top(struct pque *pq)
{
    if (!pq || pq->size == 0) {
        return NULL;
    }
    return pq->heap[0];
}

vptr pque_pop(struct pque *pq)
{
    if (!pq || pq->size == 0) {
        return NULL;
    }

    vptr min_key = pq->heap[0];
    pq->size--;
    pq->heap[0] = pq->heap[pq->size];

    for (uofs i = 0; ; ) {
        uofs imin = i;
        uofs j1 = 2 * i + 1;
        uofs j2 = j1 + 1;
        if (j1 < pq->size && pq->cmpf(pq->heap[imin], pq->heap[j1]) > 0) {
            imin = j1;
        }
        if (j2 < pq->size && pq->cmpf(pq->heap[imin], pq->heap[j2]) > 0) {
            imin = j2;
        }
        if (imin == i) {
            break;
        }
        vptr temp = pq->heap[i];
        pq->heap[i] = pq->heap[imin];
        pq->heap[imin] = temp;
        i = imin;
    }

    return min_key;
}

void pque_update(struct pque *pq, vptr key)
{
}
