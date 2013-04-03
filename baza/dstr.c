#include "dstr.h"

#define MIN_CAPACITY 23
#define DSTR(s) ((struct dstr*)((char*)s - sizeof(struct dstr)))
#define DSTR_CSTR(s) ((s)->buf)

struct dstr {
    iref len;
    iref capacity;
    char buf[];
};

char *dstr_dup(const char *strz)
{
    iref len = strlen(strz);
    iref capacity = len < MIN_CAPACITY? MIN_CAPACITY: len;
    struct dstr *ds = malloc(sizeof(struct dstr) + capacity + 1);
    ds->len = len;
    ds->capacity = capacity;
    memcpy(ds->buf, strz, len + 1);
    return DSTR_CSTR(ds);
}

char *dstr_new(iref capacity)
{
    if (capacity < MIN_CAPACITY) {
        capacity = MIN_CAPACITY;
    }
    struct dstr *ds = malloc(sizeof(struct dstr) + capacity + 1);
    ds->capacity = capacity;
    ds->len = 0;
    ds->buf[0] = 0;
    return DSTR_CSTR(ds);
}

iref dstr_len(const char *s)
{
    return DSTR(s)->len;
}

void dstr_free(const char *s)
{
    if (s) {
        free(DSTR(s));
    }
}

char *dstr_clear(char *s)
{
    struct dstr *ds = DSTR(s);
    ds->len = 0;
    ds->buf[0] = 0;
    return s;
}
