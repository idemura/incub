#include "dstr.h"

#define MIN_CAPACITY 23
#define DSTR(s) ((struct dstr*)((char*)s - sizeof(struct dstr)))
#define DSTR_CSTR(s) ((s)->str)

struct dstr {
    size_t len;
    size_t capacity;
    char str[];
};

char *dstr_dup(const char *strz)
{
    size_t len = strlen(strz);
    size_t capacity = len < MIN_CAPACITY? MIN_CAPACITY: len;
    struct dstr *ds = malloc(sizeof(struct dstr) + capacity + 1);
    ds->len = len;
    ds->capacity = capacity;
    memcpy(ds->str, strz, len + 1);
    return DSTR_CSTR(ds);
}

char *dstr_new(size_t capacity)
{
    if (capacity < MIN_CAPACITY) {
        capacity = MIN_CAPACITY;
    }
    struct dstr *ds = malloc(sizeof(struct dstr) + capacity + 1);
    ds->len = 0;
    ds->capacity = capacity;
    ds->str[0] = 0;
    return DSTR_CSTR(ds);
}

size_t dstr_len(const char *ds)
{
    return DSTR(ds)->len;
}

void dstr_free(const char *ds)
{
    if (ds) {
        free(DSTR(ds));
    }
}
