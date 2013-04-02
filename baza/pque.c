#include "pque.h"

#define DEF_CAPACITY 24

struct pque {
    void **heap;
    pque_compare cmpf;
    size_t size;
    size_t capacity;
};

struct pque *pque_create(pque_compare cmpf, size_t capacity)
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

size_t pque_size(struct pque *pq)
{
    return pq->size;
}

void pque_insert(struct pque *pq, pque_key key)
{
}

pque_key pque_pop(struct pque *pq)
{
    return NULL;
}

void pque_update(struct pque *pq, pque_key key)
{
}
