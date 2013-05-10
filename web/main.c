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
#include "scgilib.h"
#include <time.h>

int send_text(scgi_request *req, response_t *r)
{
    resp_set_content_type(r, CONTENT_TYPE_TEXT);
    resp_set_status(r, 200);
    char *buffer = resp_buffer(r);
    resp_free(r);
    int rc = scgi_write(req, buffer);
    resp_buffer_free(buffer);
    return rc;
}

int http_404(scgi_request *req)
{
    return 0;
}

int home(scgi_request *req, response_t *r)
{
    resp_printf(r, "Hello magic %d", 13);
    return send_text(req, r);
}

int handle_request(scgi_request *req)
{
    int rc = 0;

    response_t r;
    resp_init(&r);

    printf( "query string: %s\n", req->query_string );
    printf( "uri: %s\n", req->request_uri );

    char response[] =
            "Status: 200\r\n"
            "Content-Type: text/plain\r\n"
            "\r\n"
            "scgi works! new mode";
    if (req->request_method == SCGI_METHOD_GET) {
        if (strcmp(req->request_uri, "/") == 0) {
            return home(req);
        }
    }
    return scgi_write(req, response);
}

int main(int argc, char **argv)
{
    const int max_connections_accept = 5;

    int port = 9000;
    if (!scgi_initialize(port)) {
        fprintf(stderr, "SCGI init on port %d failed\n", port);
        return -1;
    }

    printf("SCGI started on port %d\n", port);

    // Enter an infinite loop, serving up responses to SCGI connections forever.
    for (;;) {
        int c;

        // Check for connections every `sleep_time` microseconds.
        // A typical server will spend the vast majority of its time sleeping,
        // too large will cause Internal Server Error due timeout.
        unsigned int sleep_time = 10; // Milliseconds
        struct timespec ts = {
            .tv_sec = 0,
            .tv_nsec = sleep_time * 1000000
        };
        nanosleep(&ts, NULL);

        for (c = 0; c < max_connections_accept; ++c) {
            int dead = 0;
            scgi_request *req = scgi_recv();
            if (!req) {
                break;
            }

            req->dead = &dead;
            handle_request(req);
            if (!dead) {
                req->dead = NULL;
            }
        }
    }
}
