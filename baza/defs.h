#ifndef DEFS_H_
#define DEFS_H_

#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <memory.h>

#define ARRAY_SIZE(x) (sizeof(x) / sizeof((x)[0]))
#define FIELD_SIZEOF(s, f) (sizeof(((s*)0)->f))

#ifdef NDEBUG
#define DEBUG 0
#else
#define DEBUG 1
#endif

typedef size_t uofs;
typedef void *vptr;

typedef int (*compare_fn)(vptr k1, vptr k2);

vptr mem_alloc(uofs size);
void mem_free(vptr p);
uofs mem_total();

#endif
