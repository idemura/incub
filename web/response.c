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

void resp_init(response *r)
{
    r->status = 200;
    r->content_type = CONTENT_TYPE_TEXT;
    r->buf_size = 2048;
    r->buf_written = 0;
    r->buf = mem_alloc(r->buf_size);
    r->buf[0] = 0;
}

void resp_free(response *r)
{
    mem_free(r->buf);
    r->buf = 0;
    r->buf_size = r->buf_written = 0;
}

char *resp_data(response *r)
{
    return r->buf;
}

void resp_printf(response *r, const char *fmt, ...)
{
    va_list va;
    va_start(va, fmt);
    uofs cb_free = r->buf_size - r->buf_written;
    int rc = vsnprintf(r->buf + r->buf_written, cb_free, fmt, va);
    if (rc >= 0) {
        if (rc + 1 > cb_free) {
            uofs min_size = r->buf_written + rc + 1;
            r->buf_size *= 2;
            if (r->buf_size < min_size) {
                uofs align_on = 1024; // Power of two
                r->buf_size = (min_size + align_on - 1) & ~(align_on - 1);
            }
            char *buf = r->buf;
            r->buf = mem_alloc(r->buf_size);
            memcpy(r->buf, buf, r->buf_written + 1);
            mem_free(buf);
            cb_free = r->buf_size - r->buf_written;
            rc = vsnprintf(r->buf + r->buf_written, cb_free, fmt, va);
        }
        r->buf_written += rc;
    }
    va_end(va);
}

void resp_set_content_type(response *r, int content_type)
{
    r->content_type = content_type;
}

void resp_set_status(response *r, int status)
{
    r->status = status;
}

char *resp_buffer(response *r)
{
    static const char *s_content_types[CONTENT_TYPE_COUNT] = {
        "text/plain",
        "text/html",
    };
    uofs buf_size = 1024;
    char *buf = mem_alloc(buf_size + r->buf_size);
    int rc = snprintf(buf, buf_size,
        "Status: %d\r\n"
        "Content-Type: %s\r\n"
        "\r\n",
        r->status, s_content_types[r->content_type]);
    if (rc < 0) {
        mem_free(buf);
        return 0;
    }
    memcpy(buf + rc, r->buf, r->buf_written + 1);
    return buf;
}

void resp_buffer_free(char *buf)
{
    mem_free(buf);
}
