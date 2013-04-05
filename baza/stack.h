#ifndef STACK_H_
#define STACK_H_

#include "defs.h"

struct stack {
    uofs buf_auto[32]; // 512 bytes on x64
    uofs *buf_heap;
    uofs *top;
};

void stack_alloc(struct stack *st, uofs capacity);
void stack_free(struct stack *st);
void stack_pushi(struct stack *st, uofs x);
void stack_pushv(struct stack *st, vptr x);
uofs stack_popi(struct stack *st);
vptr stack_popv(struct stack *st);
uofs stack_topi(struct stack *st);
vptr stack_topv(struct stack *st);
bool stack_empty(struct stack *st);

#endif
