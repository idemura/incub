#include "pque.h"

#define DEF_CAPACITY 24

struct pque {
    void **heap;
    pque_compare cmpf;
    idx size;
    idx capacity;
};

struct pque *pque_create(pque_compare cmpf, idx capacity)
{
    assert(cmpf);
    if (!cmpf) {
        return NULL;
    }
    if (capacity == 0) {
        capacity = DEF_CAPACITY;
    }
    struct pque *pq = malloc(sizeof(struct pque));
    if (!pq) {
        return NULL;
    }
    pq->heap = malloc(capacity * sizeof(void*));
    if (!pq->heap) {
        free(pq);
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
        free(pq->heap);
        free(pq);
    }
}

idx pque_size(struct pque *pq)
{
    return pq? pq->size: 0;
}

static void pque_heapify(struct pque *pq, idx i)
{
    while (i != 0) {
        idx p = (i - 1) / 2;
        if (!pq->cmpf(pq->heap[p], pq->heap[i])) {
            pque_key temp = pq->heap[p];
            pq->heap[p] = pq->heap[i];
            pq->heap[i] = temp;
        }
        i = p;
    }
}

void pque_insert(struct pque *pq, pque_key key)
{
    if (pq->size == pq->capacity) {
        // TODO: Reallocate, and copy
        assert(false);
        return;
    }

    pq->heap[pq->size] = key;
    pque_heapify(pq, pq->size);
    pq->size += 1;
}

pque_key pque_pop(struct pque *pq)
{
    if (!pq || pq->size == 0) {
        return NULL;
    }

    pque_key min_key = pq->heap[0];
    pq->heap[0] = pq->heap[pq->size - 1];
    pq->size -= 1;

    for (idx i = 0; ; ) {
        idx imin = i;
        idx j1 = 2 * i + 1;
        idx j2 = j1 + 1;
        if (j1 < pq->size && !pq->cmpf(pq->heap[i], pq->heap[j1])) {
            imin = j1;
        }
        if (j2 < pq->size && !pq->cmpf(pq->heap[i], pq->heap[j2])) {
            imin = j2;
        }
        if (imin == i) {
            break;
        }
        pque_key temp = pq->heap[i];
        pq->heap[i] = pq->heap[imin];
        pq->heap[imin] = temp;
        i = imin;
    }

    return min_key;
}

void pque_update(struct pque *pq, pque_key key)
{
}
