#include "pque.h"

struct pque {
    vptr *heap;
    pque_compare cmpf;
    iref size;
    iref capacity;
};

struct pque *pque_create(pque_compare cmpf, iref capacity)
{
    assert(cmpf);
    if (!cmpf) {
        return NULL;
    }
    struct pque *pq = malloc(sizeof(struct pque));
    if (!pq) {
        return NULL;
    }
    if (capacity < 1) {
        capacity = 1;
    }
    pq->heap = malloc(capacity * sizeof(vptr));
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

iref pque_size(struct pque *pq)
{
    return pq? pq->size: 0;
}

static void pque_heapify(struct pque *pq, iref i)
{
    while (i != 0) {
        iref p = (i - 1) / 2;
        if (!pq->cmpf(pq->heap[p], pq->heap[i])) {
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
        iref capacity = pq->capacity + pq->capacity / 2;
        if (capacity <= pq->capacity) {
            capacity += pq->capacity + 1;
        }
        void *new = malloc(capacity * sizeof(vptr));
        if (!new) {
            return;
        }
        memcpy(new, pq->heap, pq->size * sizeof(vptr));
        pq->capacity = capacity;
        pq->heap = new;
    }

    pq->heap[pq->size] = key;
    pque_heapify(pq, pq->size);
    pq->size += 1;
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
    pq->heap[0] = pq->heap[pq->size - 1];
    pq->size -= 1;

    for (iref i = 0; ; ) {
        iref imin = i;
        iref j1 = 2 * i + 1;
        iref j2 = j1 + 1;
        if (j1 < pq->size && !pq->cmpf(pq->heap[i], pq->heap[j1])) {
            imin = j1;
        }
        if (j2 < pq->size && !pq->cmpf(pq->heap[i], pq->heap[j2])) {
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
