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
#include "test.h"
#include <string.h>
#include <stdarg.h>

static const char *s_name;
static int s_failed_asserts;
static int s_failed;
static int s_passed;
static uofs s_memory;
static FILE *s_out;
static struct timeval s_start;

bool color_text(char *out, size_t out_len, const char *in)
{
    if (out_len == 0) {
        return false;
    }

    bool coloring = 0;
    while (*in && out_len > 1) {
        if (*in == '#') {
            if (in[1] == '#') {
                *out++ = '#';
                in += 2;
                continue;
            }
#ifdef _WIN32
            if (coloring) {
                in += 1;
            } else {
                in += 2;
            }
            coloring = !coloring;
#else
            char buf[16];
            if (coloring) {
                snprintf(buf, sizeof(buf), "%c[0m", 27);
                in += 1;
            } else {
                int fg = in[1] - '0';
                snprintf(buf, sizeof(buf), "%c[1;%dm", 27, fg + 30);
                in += 2;
            }
            size_t buf_len = strlen(buf);
            if (out_len <= buf_len) {
                *out = 0;
                return false;
            }
            memcpy(out, buf, buf_len);
            out_len -= buf_len;
            out += buf_len;
            coloring = !coloring;
#endif
        } else {
            *out++ = *in++;
        }
    }
    *out = 0;
    return out_len > 1;
}

void test_init()
{
    timer_get(&s_start);
}

void test_begin(const char *name)
{
    s_name = name;
    s_failed_asserts = 0;

    struct mem_stat stat;
    mem_stat(&stat);
    s_memory = stat.total;
}

void test_end()
{
    char fmt[80];
    if (s_failed_asserts == 0) {
        s_passed++;
        if (color_text(fmt, sizeof(fmt), "#2Passed# %s\n")) {
            fprintf(test_out(), fmt, s_name);
        }
    } else {
        s_failed++;
        if (color_text(fmt, sizeof(fmt), "#1FAILED# %s\n")) {
            fprintf(test_out(), fmt, s_name);
        }
    }

    struct mem_stat stat;
    mem_stat(&stat);
    if (stat.total != s_memory) {
        if (color_text(fmt, sizeof(fmt), "#3Warning# %s memory leak: %zu\n")) {
            fprintf(test_out(), fmt, s_name, stat.total - s_memory);
        }
    }
}

void test_check(int ok, const char *expr, const char *file, int line,
    const char *format, ...)
{
    char fmt[80];
    if (!ok) {
        if (format) {
            if (color_text(fmt, sizeof(fmt), "#1FAILED# in %s %s:%d: ")) {
                fprintf(test_out(), fmt, s_name, file, line, expr);
            }
            if (format) {
                va_list va;
                va_start(va, format);
                vfprintf(test_out(), format, va);
                va_end(va);
            }
            fprintf(test_out(), "\n");
        } else {
            if (color_text(fmt, sizeof(fmt), "#1FAILED# in %s %s:%d: %s\n")) {
                fprintf(test_out(), fmt, s_name, file, line, expr);
            }
        }
        s_failed_asserts++;
    }
}

void test_report()
{
    char fmt[80];
    int64_t usec = timer_int(&s_start);
    if (s_failed == 0) {
        if (color_text(fmt, sizeof(fmt), "Tests: %d #2passed#\n")) {
            fprintf(test_out(), fmt, s_passed);
        }
    } else {
        if (color_text(fmt, sizeof(fmt), "Tests: %d #2passed#, %d #1FAILED#!\n")) {
            fprintf(test_out(), fmt, s_passed, s_failed);
        }
    }
    fprintf(test_out(), "Done in %i ms\n", (int)(usec / 1000));
}

int test_failed_count()
{
    return s_failed;
}

int test_passed_count()
{
    return s_passed;
}

FILE *test_out()
{
    if (!s_out) {
        s_out = stderr;
    }
    return s_out;
}

void test_setout(FILE *f)
{
    s_out = f;
}
