#ifndef DSTR_H_
#define DSTR_H_

#include "defs.h"
#include <string.h>

char *dstr_dup(const char *strz);
char *dstr_new(uofs capacity);
uofs dstr_len(const char *s);
void dstr_free(const char *s);
char *dstr_clear(char *s);

#endif
