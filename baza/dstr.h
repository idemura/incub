#ifndef DSTR_H_
#define DSTR_H_

#include <stdlib.h>
#include <string.h>

char *dstr_dup(const char *strz);
char *dstr_new(size_t capacity);
size_t dstr_len(const char *ds);
void dstr_free(const char *ds);

#endif
