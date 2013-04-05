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
#include "defs.h"

#define MEM_ALIGN sizeof(uofs)
#ifdef DEBUG
#define MEM_PAD MEM_ALIGN
#else
#define MEM_PAD 0
#endif

struct mem_block {
    uofs size;
    unsigned char p[];
};

static uofs mem_bytes;

vptr mem_alloc(uofs size)
{
    size = (size + (MEM_ALIGN - 1)) & ~(MEM_ALIGN - 1);
    struct mem_block *mb = malloc(sizeof(struct mem_block) + size + MEM_PAD);
    if (!mb) {
        return NULL;
    }
    mb->size = size;
    mem_bytes += mb->size;
#ifdef DEBUG
    memset(mb->p, 0xcc, size + MEM_PAD);
#endif
    return mb->p;
}

void mem_free(vptr p)
{
    if (!p) {
        return;
    }
    struct mem_block *mb = (void*)((char*)p - sizeof(struct mem_block));
    mem_bytes -= mb->size;
#ifdef DEBUG
    for (uofs i = 0; i < MEM_PAD; ++i) {
        assert(mb->p[mb->size + i] == 0xcc);
    }
#endif
    free(mb);
}

uofs mem_total()
{
    return mem_bytes;
}
