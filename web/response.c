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
#include "response.h"
#include <stdarg.h>

void buf_init(buffer *b)
{
    b->size = 2048;
    b->written = 0;
    b->data = mem_alloc(b->size);
    b->data[0] = 0;
}

void buf_free(buffer *b)
{
    mem_free(b->data);
    b->data = 0;
    b->size = b->written = 0;
}

char *buf_data(buffer *b)
{
    return b->data;
}

void buf_printf(buffer *b, const char *fmt, ...)
{
    va_list va;
    va_start(va, fmt);
    uofs cb_free = b->size - b->written;
    int rc = vsnprintf(b->data + b->written, cb_free, fmt, va);
    if (rc >= 0) {
        if (rc + 1 > cb_free) {
            uofs min_size = b->written + rc + 1;
            b->size *= 2;
            if (b->size < min_size) {
                uofs align_on = 1024; // Power of two
                b->size = (min_size + align_on - 1) & ~(align_on - 1);
            }
            char *data = b->data;
            b->data = mem_alloc(b->size);
            memcpy(b->data, data, b->written + 1);
            mem_free(data);
            cb_free = b->size - b->written;
            rc = vsnprintf(b->data + b->written, cb_free, fmt, va);
        }
        b->written += rc;
    }
    va_end(va);
}

/**
 * Response functions.
**/
void resp_init(response *r)
{
    buf_init(&r->buf);
    r->status = 200;
    r->content_type = CONTENT_TYPE_TEXT;
}

void resp_free(response *r)
{
    buf_free(&r->buf);
}

buffer *resp_buffer(response *r)
{
    return &r->buf;
}

void resp_set_content_type(response *r, int content_type)
{
    r->content_type = content_type;
}

void resp_set_status(response *r, int status)
{
    r->status = status;
}

char *resp_output(response *r)
{
    static const char *s_content_types[CONTENT_TYPE_COUNT] = {
        "text/plain",
        "text/html",
    };
    const char *status_desc = "";
    switch (r->status) {
    case 200:
        status_desc = "OK";
        break;
    case 404:
        status_desc = "Not Found";
        break;
    case 500:
        status_desc = "Internal Server Error";
        break;
    case 501:
        status_desc = "Not Implemented";
        break;
    case 503:
        status_desc = "Service Unavailable";
        break;
    }
    uofs buf_size = 1024;
    char *buf = mem_alloc(buf_size + r->buf.size);
    int rc = snprintf(buf, buf_size,
        "Status: %d %s\r\n"
        "Content-Type: %s\r\n"
        "\r\n",
        r->status, status_desc, s_content_types[r->content_type]);
    if (rc < 0) {
        mem_free(buf);
        return 0;
    }
    memcpy(buf + rc, r->buf.data, r->buf.written + 1);
    return buf;
}

void resp_output_free(char *buf)
{
    mem_free(buf);
}
