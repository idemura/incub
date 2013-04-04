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
