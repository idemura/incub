#include "test.h"
#include "defs.h"
#include <stdio.h>
#include <string.h>

static const char *s_name;
static int s_failed_asserts;
static int s_failed;
static int s_passed;

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
}

void test_begin(const char *name)
{
    s_name = name;
    s_failed_asserts = 0;
}

void test_end()
{
    char fmt[80];
    if (s_failed_asserts == 0) {
        s_passed++;
        if (color_text(fmt, sizeof(fmt), "#2Passed# %s\n")) {
            fprintf(stderr, fmt, s_name);
        }
    } else {
        s_failed++;
        if (color_text(fmt, sizeof(fmt), "#1FAILED# %s\n")) {
            fprintf(stderr, fmt, s_name);
        }
    }
}

void test_check(int ok, const char *expr, const char *file, int line)
{
    char fmt[80];
    if (!ok) {
        if (color_text(fmt, sizeof(fmt), "#1FAILED# in %s %s:%d: %s\n")) {
            fprintf(stderr, fmt, s_name, file, line, expr);
        }
        s_failed_asserts++;
    }
}

int test_report()
{
    char fmt[80];
    if (s_failed == 0) {
        if (color_text(fmt, sizeof(fmt), "Tests: %d #2passed#\n")) {
            fprintf(stderr, fmt, s_passed);
        }
    } else {
        if (color_text(fmt, sizeof(fmt), "Tests: %d #2passed#, %d #1FAILED#!\n")) {
            fprintf(stderr, fmt, s_passed, s_failed);
        }
    }
    return s_failed == 0? 0: 1;
}
