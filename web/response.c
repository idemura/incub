#include "response.h"
#include "scgilib.h"
#include <stdarg.h>

void resp_init(response_t *r)
{
    r->status = 200;
    r->content_type = CONTENT_TYPE_TEXT;
    r->buf_size = 4096;
    r->buf_written = 0;
    r->buf = mem_alloc(r->buf_size);
}

void resp_free(response_t *r)
{
    mem_free(r->buf);
    r->buf = 0;
    r->buf_size = r->buf_written = 0;
}

void resp_printf(response_t *r, const char *fmt, ...)
{
    // XXX:
    va_list va;
    va_start(va, fmt);
    uofs len = vsnprintf(r->buf + r->buf_written, r->buf_size - r->buf_written,
        fmt, va) + 1;
    if (r->buf_written + len > r->buf_size) {
        r->buf_size = 2 * r->buf_size; // max to written + len!
        r->buf = mem_alloc(r->buf_size);
    }
    va_end(va);
}

void resp_set_content_type(response_t *r, int content_type)
{
    r->content_type = content_type;
}

void resp_set_status(response_t *r, int status)
{
    r->status = status;
}

char *resp_buffer(response_t *r)
{
    // Put header and text together and send this buffer.
    return 0;
}

void resp_buffer_free(char *buf)
{
    mem_free(buf);
}
