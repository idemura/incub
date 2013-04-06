/*
  Copyright 2013 Igor Demura

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
*/
#include "stack.h"

void stack_alloc(struct stack *st, uofs capacity)
{
    if (capacity > AUTO_STACK_SIZE) {
        st->buf = mem_alloc(capacity * sizeof(uofs));
        st->top = st->buf;
        st->bottom = st->top + capacity;
    } else {
        st->buf = st->buf_auto;
        st->top = st->buf;
        st->bottom = st->top + AUTO_STACK_SIZE;
    }
}

void stack_free(struct stack *st)
{
    if (st->buf != st->buf_auto) {
        mem_free(st->buf);
    }
    st->buf = NULL;
}

void stack_pushi(struct stack *st, uofs x)
{
    if (st->top == st->bottom) {
        const uofs size = st->bottom - st->buf;
        const uofs new_size = 2 * size;
        uofs *new = mem_alloc(new_size * sizeof(uofs));
        memcpy(new, st->buf, size * sizeof(uofs));
        stack_free(st);
        stack_free(st);
        st->buf = new;
        st->top = new + size;
        st->bottom = new + new_size;
    }
    *st->top = x;
    st->top++;
}

uofs stack_popi(struct stack *st)
{
    assert(!stack_empty(st));
    st->top--;
    return *st->top;
}

uofs stack_topi(struct stack *st)
{
    assert(!stack_empty(st));
    return st->top[-1];
}

void stack_pushv(struct stack *st, vptr x)
{
    stack_pushi(st, (uofs)x);
}

vptr stack_popv(struct stack *st)
{
    return (vptr)stack_popi(st);
}

vptr stack_topv(struct stack *st)
{
    return (vptr)stack_topi(st);
}

bool stack_empty(struct stack *st)
{
    return stack_size(st) == 0;
}

uofs stack_size(struct stack *st)
{
    assert(st->top >= st->buf);
    return st->top - st->buf;
}
