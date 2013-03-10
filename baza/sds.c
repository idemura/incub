#include "sds.h"

#include <string.h>
#include <stdio.h>

#define MIN_CAPACITY 23
#define SDS(s) (struct string*)((char*)s - sizeof(struct string))
#define SDSSTRZ(s) ((s)->str)

struct string {
    int len;
    int capacity;
    char str[];
};

char *sdsdupz(const char *strz)
{
    struct string *sds;
    size_t len = strlen(strz);
    size_t capacity = len < MIN_CAPACITY? MIN_CAPACITY: len;

    sds = malloc(sizeof(struct string) + capacity + 1);
    sds->len = len;
    sds->capacity = capacity;
    memcpy(sds->str, strz, len + 1);
    return SDSSTRZ(sds);
}

char *sdsempty(size_t capacity)
{
    struct string *sds;
    if (capacity < MIN_CAPACITY) {
        capacity = MIN_CAPACITY;
    }
    sds = malloc(sizeof(struct string) + capacity + 1);
    sds->len = 0;
    sds->capacity = capacity;
    sds->str[0] = 0;
    return SDSSTRZ(sds);
}

size_t sdslen(const char *s)
{
    struct string *sds = SDS(s);
    return sds->len;
}

void sdsfree(const char *s)
{
    if (s) {
        free(SDS(s));
    }
}
