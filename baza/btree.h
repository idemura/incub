#ifndef BTREE_H_
#define BTREE_H_

#include "defs.h"

typedef uofs key_t;

struct btree;

struct btree *btree_create(int min_keys);
void btree_destroy(struct btree *bt);
uofs btree_size(struct btree *bt);
void btree_insert(struct btree *bt, key_t key, vptr value);
vptr btree_find(struct btree *bt, key_t key);

#endif
