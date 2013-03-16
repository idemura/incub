#ifndef BTREE_H_
#define BTREE_H_

#include "defs.h"

typedef int int_key;

struct btree;

struct btree *btree_create(int min_keys);
void btree_destroy(struct btree *bt);
size_t btree_size(struct btree *bt);
void btree_insert(struct btree *bt, int_key key, void *value);
void *btree_find(struct btree *bt, int_key key);
size_t btree_memory();

#endif
