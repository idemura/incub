#ifndef PQUE_H_
#define PQUE_H_

#include "defs.h"

struct pque;

typedef void *pque_key;
typedef bool (*pque_compare)(pque_key k1, pque_key k2);

struct pque *pque_create(pque_compare cmpf, idx capacity);
void    pque_destroy(struct pque *pq);
idx     pque_size(struct pque *pq);
void    pque_insert(struct pque *pq, pque_key key);
pque_key pque_pop(struct pque *pq);
void    pque_update(struct pque *pq, pque_key key);

#endif
