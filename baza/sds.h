#ifndef SDS_H_
#define SDS_H_

#include <stdlib.h>

char *sdsdup(const char *strz);
char *sdsempty(size_t capacity);
size_t sdslen(const char *s);

#endif
