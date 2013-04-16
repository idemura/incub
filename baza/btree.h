#ifndef BTREE_H_
#define BTREE_H_

/*
  B+ tree implementation. It's memory only yet. Disk is in future plans.
*/

#include "defs.h"
#include "disk.h"

struct btree;
struct btree_node;

struct btree_iter {
    struct btree_node *node;
    int j;
};

struct btree *btree_create(struct disk_io *io, const char *name,
    compare_fn cmpf, int min_keys);

void btree_destroy(struct btree *bt);
uofs btree_size(struct btree *bt);
void btree_insert(struct btree *bt, vptr key, vptr value);
bool btree_find(struct btree *bt, vptr key, struct btree_iter *iter);
vptr btree_iter_key(struct btree_iter *iter);
vptr btree_iter_value(struct btree_iter *iter);
bool btree_iter_next(struct btree_iter *iter);
bool btree_iter_prev(struct btree_iter *iter);

#endif
