#ifndef TYPES_H_
#define TYPES_H_

#include <stdint.h>

#define ARRAY_SIZE(x) (sizeof(x) / sizeof((x)[0]))
#define FIELD_SIZEOF(t, f) (sizeof(((t*)0)->f))

const char *prod_name();

#endif
