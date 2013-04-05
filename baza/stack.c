#include "stack.h"

void stack_alloc(struct stack *st, uofs capacity)
{
    if (capacity > FIELD_SIZEOF(struct stack, buf_auto)) {
        st->buf_heap = mem_alloc(capacity * sizeof(uofs));
        st->top = st->buf_heap;
    } else {
        st->buf_heap = NULL;
        st->top = st->buf_auto;
    }
}

void stack_free(struct stack *st)
{
    if (st->buf_heap) {
        mem_free(st->buf_heap);
    }
}

void stack_pushi(struct stack *st, uofs x)
{
    *st->top = x;
    st->top++;
}

uofs stack_popi(struct stack *st)
{
    assert(!stack_empty(st));
    st->top--;
    return *st->top;
}

void stack_pushv(struct stack *st, vptr x)
{
    stack_pushi(st, (uofs)x);
}

vptr stack_popv(struct stack *st)
{
    return (vptr)stack_popi(st);
}

bool stack_empty(struct stack *st)
{
    return st->top == st->buf_auto || st->top == st->buf_heap;
}
