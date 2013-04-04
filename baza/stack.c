#include "stack.h"

void stack_alloc(struct stack *st, iref capacity)
{
    if (capacity > FIELD_SIZEOF(struct stack, mem_auto)) {
        st->mem_heap = malloc(capacity * sizeof(iref));
        st->top = st->mem_heap;
    } else {
        st->mem_heap = NULL;
        st->top = st->mem_auto;
    }
}

void stack_free(struct stack *st)
{
    if (st->mem_heap) {
        free(st->mem_heap);
    }
}

void stack_push(struct stack *st, iref x)
{
    *st->top = x;
    st->top++;
}

iref stack_pop(struct stack *st)
{
    st->top--;
    return *st->top;
}

bool stack_empty(struct stack *st)
{
    return st->top == st->mem_auto || st->top == st->mem_heap;
}
