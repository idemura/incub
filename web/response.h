#ifndef RESPONSE_H_
#define RESPONSE_H_

#include "defs.h"

typedef struct {
    int status;
    int content_type;
    uofs buf_size;
    uofs buf_written;
    char *buf;
} response;

enum {
    CONTENT_TYPE_TEXT,
    CONTENT_TYPE_HTML,
    CONTENT_TYPE_COUNT,
};

void  resp_init(response *r);
void  resp_free(response *r);
char *resp_data(response *r);
void  resp_printf(response *r, const char *fmt, ...);
void  resp_set_content_type(response *r, int content_type);
void  resp_set_status(response *r, int status);
char *resp_buffer(response *r);
void  resp_buffer_free(char *buf);

#endif
