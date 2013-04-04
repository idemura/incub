#ifndef DEFS_H_
#define DEFS_H_

#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

typedef size_t uofs;
typedef void *vptr;

#define ARRAY_SIZE(x) (sizeof(x) / sizeof((x)[0]))
#define FIELD_SIZEOF(s, f) (sizeof(((s*)0)->f))

vptr mem_alloc(uofs size);
void mem_free(vptr p);
uofs mem_total();

#endif
