#ifndef STACK_H_
#define STACK_H_

#include "defs.h"

struct stack {
    iref buf_auto[64]; // 512 bytes on x64
    iref *buf_heap;
    iref *top;
};

void stack_alloc(struct stack *st, iref capacity);
void stack_free(struct stack *st);
void stack_pushi(struct stack *st, iref x);
iref stack_popi(struct stack *st);
void stack_pushv(struct stack *st, vptr x);
vptr stack_popv(struct stack *st);
bool stack_empty(struct stack *st);

#endif
