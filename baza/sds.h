#ifndef SDS_H_
#define SDS_H_

#include <stdlib.h>
#include <string.h>

char *sdsdupz(const char *strz);
char *sdsempty(size_t capacity);
size_t sdslen(const char *s);
void sdsfree(const char *s);

#endif
