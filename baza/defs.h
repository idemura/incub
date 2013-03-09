#ifndef DEFS_H_
#define DEFS_H_

#include <stdint.h>
#include <stdlib.h>
#include <stddef.h>

#define ARRAY_SIZE(x) (sizeof(x) / sizeof((x)[0]))
#define FIELD_SIZEOF(t, f) (sizeof(((t*)0)->f))

#endif
