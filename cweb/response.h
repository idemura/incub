#ifndef RESPONSE_H_
#define RESPONSE_H_

typedef struct {
    int status;
    int content_type;
    uofs buf_size;
    char *buf;
} response_t;

enum {
    CONTENT_TYPE_TEXT,
    CONTENT_TYPE_HTML,
};

void resp_init(response_t *r);
void resp_printf(response_t *r, const char *fmt, ...);
void resp_set_content_type(response_t *r, int content_type);
void resp_set_status(response_t *r, int status);
int  resp_send(response_t *r);

#endif
