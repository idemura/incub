#ifndef BTREE_H_
#define BTREE_H_

#include "defs.h"

struct btree;

struct btree *btree_create(compare_fn cmpf, int min_keys);
void btree_destroy(struct btree *bt);
uofs btree_size(struct btree *bt);
void btree_insert(struct btree *bt, vptr key, vptr value);
vptr btree_find(struct btree *bt, vptr key);

#endif
