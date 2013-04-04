#ifndef PQUE_H_
#define PQUE_H_

#include "defs.h"

struct pque;

typedef bool (*pque_compare)(vptr k1, vptr k2);

struct pque *pque_create(pque_compare cmpf, iref capacity);
void pque_destroy(struct pque *pq);
iref pque_size(struct pque *pq);
void pque_insert(struct pque *pq, vptr key);
vptr pque_top(struct pque *pq);
vptr pque_pop(struct pque *pq);
void pque_update(struct pque *pq, vptr key);

#endif
