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

uofs stack_topi(struct stack *st)
{
    assert(!stack_empty(st));
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

vptr stack_topv(struct stack *st)
{
    return (vptr)stack_topi(st);
}

bool stack_empty(struct stack *st)
{
    return st->top == st->buf_auto || st->top == st->buf_heap;
}
