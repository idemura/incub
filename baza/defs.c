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
#include <stdio.h>

#define MEM_ALIGN sizeof(uofs)
#if DEBUG
#define MEM_PAD MEM_ALIGN
#else
#define MEM_PAD 0
#endif

struct mem_block {
    uofs size;
    unsigned char p[];
};

static uofs mem_bytes;

static uofs mem_adjust(uofs size)
{
    return ((size + (MEM_ALIGN - 1)) & ~(MEM_ALIGN - 1)) + MEM_PAD;

}

vptr mem_alloc(uofs size)
{
    uofs adjusted = mem_adjust(size);
    struct mem_block *mb = malloc(sizeof(struct mem_block) + adjusted);
    if (!mb) {
        fprintf(stderr,
                "OUT OF HEAP MEMORY\n"
                "  Total allocated: %zu\n"
                "  Requested: %zu\n", mem_bytes, size);
        exit(1);
        return NULL;
    }
    mb->size = size;
    mem_bytes += mb->size;
#if DEBUG
    memset(mb->p, 0xcc, adjusted);
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
#if DEBUG
    for (uofs i = mb->size, n = mem_adjust(mb->size); i < n; ++i) {
        assert(mb->p[i] == 0xcc);
    }
#endif
    free(mb);
}

uofs mem_total()
{
    return mem_bytes;
}
