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
#include <stdarg.h>
#include <string.h>

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

static struct mem_stat smem_stat;
static FILE *slog_file;
static bool  slog_newline = true;
static uint32_t slog_flag = LOG_TIMESTAMP;

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
            "  Total allocated: %zu in %zu instances\n"
            "  Requested: %zu\n",
            smem_stat.total, smem_stat.instances,
            size);
        exit(1);
        return NULL;
    }
    mb->size = size;
#if DEBUG
    memset(mb->p, 0xcc, adjusted);
#endif
    smem_stat.instances++;
    smem_stat.total += mb->size;
    return mb->p;
}

void mem_free(vptr p)
{
    if (!p) {
        return;
    }
    struct mem_block *mb = (void*)((char*)p - sizeof(struct mem_block));
#if DEBUG
    uofs i, n = mem_adjust(mb->size);
    for (i = mb->size; i < n; ++i) {
        assert(mb->p[i] == 0xcc);
    }
#endif
    smem_stat.instances--;
    smem_stat.total -= mb->size;
    free(mb);
}

void mem_stat(struct mem_stat *stat)
{
    *stat = smem_stat;
}

void log_setfile(FILE *f)
{
    slog_file = f;
}

FILE *log_getfile()
{
    return slog_file;
}

bool log_flag(uint32_t prop, bool val)
{
    bool ret = (slog_flag & prop) != 0;
    if (val) {
        slog_flag |= prop;
    } else {
        slog_flag &= ~prop;
    }
    return ret;
}

void log_print(const char* format, ...)
{
    if (!slog_file) {
        slog_file = stdout;
    }

    if (slog_flag & LOG_TIMESTAMP) {
        if (slog_newline) {
            struct timeval tv;
            timer_get(&tv);
            fprintf(slog_file, "%2li.%03i: ", tv.tv_sec,
                (int)(tv.tv_usec / 1000));
        }
    }

    va_list va;
    va_start(va, format);
    vfprintf(slog_file, format, va);
    va_end(va);

    size_t format_len = strlen(format);
    slog_newline = format_len > 0 && format[format_len - 1] == '\n';
}

static void timer_diff(struct timeval *end, struct timeval *start,
    struct timeval *res)
{
    res->tv_sec = end->tv_sec - start->tv_sec;
    res->tv_usec = end->tv_usec - start->tv_usec;
    if (res->tv_usec < 0) {
        res->tv_usec += 1000000;
        res->tv_sec -= 1;
    }
}

void timer_get(struct timeval *tv)
{
    static struct timeval s_init;
    gettimeofday(tv, NULL);
    if (s_init.tv_sec == 0) {
        s_init = *tv;
    }
    timer_diff(tv, &s_init, tv);
}

int64_t timer_sub(struct timeval *end, struct timeval *start)
{
    struct timeval sub;
    timer_diff(end, start, &sub);
    return (uint64_t)sub.tv_sec * 1000000 + sub.tv_usec;
}

int64_t timer_int(struct timeval *start)
{
    struct timeval end;
    timer_get(&end);
    return timer_sub(&end, start);
}
