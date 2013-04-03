#ifndef DSTR_H_
#define DSTR_H_

#include "defs.h"

char *dstr_dup(const char *strz);
char *dstr_new(iref capacity);
iref dstr_len(const char *s);
void dstr_free(const char *s);
char *dstr_clear(char *s);

#endif
