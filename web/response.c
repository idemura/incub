#include "response.h"

void resp_init(response_t *r)
{
    r->status = 200;
    r->content_type = CONTENT_TYPE_TEXT;
    r->buf_size = 4096;
    r->buf_written = 0;
    r->buf = mem_alloc(r->buf_size);
}

void resp_printf(response_t *r, const char *fmt, ...)
{
    va_list va;
    va_start(va);
    uofs len = vsnpintf(r->buf, 0, fmt, va) + 1;
    if (r->buf_written + len > buf_size) {
        r->buf = mem_alloc();
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

int resp_send(response_t *r)
{
}
