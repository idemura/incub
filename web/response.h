#ifndef RESPONSE_H_
#define RESPONSE_H_

#include "defs.h"

typedef struct {
    char *data;
    uofs written;
    uofs size;
} buffer;

typedef struct {
    int status;
    int content_type;
    buffer buf;
} response;

enum {
    CONTENT_TYPE_TEXT,
    CONTENT_TYPE_HTML,
    CONTENT_TYPE_COUNT,
};

void  buf_init(buffer *b);
void  buf_free(buffer *b);
char *buf_data(buffer *b);
void  buf_printf(buffer *b, const char *fmt, ...);

void  resp_init(response *r);
void  resp_free(response *r);
buffer *resp_buffer(response *r);
void  resp_set_content_type(response *r, int content_type);
void  resp_set_status(response *r, int status);
char *resp_output(response *r);
void  resp_output_free(char *buf);

#endif
