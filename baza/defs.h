#ifndef DEFS_H_
#define DEFS_H_

#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <memory.h>
#include <sys/time.h>
#include <stdio.h>

#define ARRAY_SIZEOF(a)     (sizeof(a) / sizeof((a)[0]))
#define FIELD_SIZEOF(s, f)  (sizeof(((s*)0)->f))
#define ZERO(p, n)          memset(p, 0, n);

#ifdef NDEBUG
#define DEBUG 0
#else
#define DEBUG 1
#endif

#define LOG_TIMESTAMP 1u

typedef size_t uofs;
typedef void *vptr;

typedef int (*compare_fn)(vptr k1, vptr k2);

struct mem_stat {
    uofs total;
    uofs instances;
};

vptr mem_alloc(uofs size);
void mem_free(vptr p);
void mem_stat(struct mem_stat *stat);

void log_print(const char* format, ...);
void  log_setfile(FILE *f);
FILE *log_getfile();
bool log_flag(uint32_t prop, bool val);

void timer_get(struct timeval *tv);
int64_t timer_sub(struct timeval *end, struct timeval *start);
int64_t timer_int(struct timeval *start);

#endif
