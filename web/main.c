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
#include "template.h"
#include "scgilib.h"
#include <time.h>

void send_response(scgi_request *req, response *r)
{
    char *buf = resp_output(r);
    scgi_write(req, buf);
    resp_output_free(buf);
}

void http_error(scgi_request *req, response *r, int err_code)
{
    resp_set_status(r, err_code);
    buf_printf(resp_buffer(r), "HTTP error %d\n", err_code);
}

void home(scgi_request *req, response *r)
{
    buf_printf(resp_buffer(r), "Hello magic %d, this is home directory\n", 13);
}

void handle_request(scgi_request *req)
{
    response r;
    resp_init(&r);

    printf("query string: '%s'\n", req->query_string);
    printf("uri: %s\n", req->request_uri);

    if (req->request_method == SCGI_METHOD_GET) {
        if (strcmp(req->request_uri, "/") == 0) {
            home(req, &r);
        } else {
            http_error(req, &r, 404);
        }
    } else {
        http_error(req, &r, 501);
    }
    send_response(req, &r);
    resp_free(&r);
}

int main(int argc, char **argv)
{
    tpl_test();
    return 0;

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
            .tv_nsec = sleep_time * 1000000,
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
