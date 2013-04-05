#ifndef PQUE_H_
#define PQUE_H_

#include "defs.h"

struct pque;

struct pque *pque_create(compare_fn cmpf, uofs capacity);
void pque_destroy(struct pque *pq);
uofs pque_size(struct pque *pq);
void pque_insert(struct pque *pq, vptr key);
vptr pque_top(struct pque *pq);
vptr pque_pop(struct pque *pq);
void pque_update(struct pque *pq, vptr key);

#endif
