#ifndef TEMPLATE_H_
#define TEMPLATE_H_

#include "defs.h"

struct buffer;

typedef struct {
    char *code;
} template;

template *tpl_parse(char *code);
void tpl_free(template *t);
int  tpl_execute(template *t, struct buffer *buffer);
void tpl_test(void);

#endif
