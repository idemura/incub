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
#include "template.h"
#include <ctype.h>
/*
Template sample:
<body>
#if (salary > 1000) && (year < 2003)
<h1>Hello your wages are so good!</h1>
#elif (year < 2012)
<p>Average Russian salary.</p>
#else
<p>You need social security?</p>
#end

<ul>
#if name_list
 <li>${name_i}
#loop
</ul>
</body>

Template can do logical ops, but can't compare, it involves type information.
*/

enum keyword {
    KW_UNKNOWN,
    KW_IF,
    KW_ELIF,
    KW_ELSE,
    KW_END,
    KW_LOOP,
    KW_NULL,
};

char *next_line(char *r)
{
    for (; *r && *r != '\n'; ++r) {
    }
    return *r ? r + 1: r;
}

int is_word(const char *s, const char *word)
{
    while (*s == *word && *word) {
        s++;
        word++;
    }
    if (*word) {
        return 0;
    }
    return !(isalnum(*s) || *s == '_');
}

int keyword(char *r, char **r_end)
{
    static char *kws[] = {
        "if",
        "elif",
        "else",
        "end",
        "loop",
        "", // Empty string
    };
    int i;

    for (i = 0; i < ARRAY_SIZEOF(kws); ++i) {
        char *kw = kws[i];
        if (is_word(r, kw)) {
            r += strlen(kw);
            *r_end = r;
            return i + 1; // See enum keyword.
        }
    }
    return KW_UNKNOWN;
}

char* parse_empty(char *r)
{
    for (; isspace(*r); ++r) {
    }
    return r;
}

int parse_cmd(template *t, char *cmd)
{
    // Strip by comment.
    char *r = cmd;
    while (*r && !(r[0] == '-' && r[1] == '-')) {
        r++;
    }
    if (*r) {
        printf("comment `%s'\n", r);
        *r = 0;
    }
    r = cmd;
    printf("cmd: `%s`\n", cmd);
    switch (keyword(r, &r)) {
    case KW_IF:
    case KW_ELIF:
        printf("condition\n");
        break;
    case KW_LOOP:
    case KW_ELSE:
    case KW_END:
    case KW_NULL:
        if (*parse_empty(r)) {
            printf("Empty params expected\n");
            return 0;
        }
        printf("terminal\n");
        break;
    default:
        printf("Unknown!!!\n");
        return 0;
    }
    return 1;
    // const char *kw = 0;
    // if (is_word(r, kw="if") || is_word(r, kw="elif")) {
    //     // parse expr
    //     r += strlen(kw);
    //     printf("keyword is %s\n", kw);
    // } else if (is_word(r, kw="else") ||
    //            is_word(r, kw="loop") ||
    //            is_word(r, kw="end")) {
    //     r += strlen(kw);
    // } else {
    //     r = parse_empty(r);
    //     if (*r) {
    //         printf("Unknown keyword\n");
    //         return 0;
    //     }
    // }
    // return 1;
}

template *tpl_parse(char *code)
{
    template *t = mem_alloc(sizeof *t);
    // Go line by line
    int errors = 0;
    int line = 1;
    char *r = code;
    char *text = r;
    while (*r) {
        char *line_start = r;
        printf("line start>>%s<<\n", line_start);
        for (; isspace(*r) && *r != '\n'; ++r) {
        }
        if (*r == '#') {
            // Null-terminate text and push to the command list.
            *line_start = 0;
            char *cmd = ++r;
            for (; *r && *r != '\n'; ++r) {
            }
            if (*r) {
                *r++ = 0;
            }
            text = r;
            if (!parse_cmd(t, cmd)) {
                printf("error in command, line %d\n", line);
                errors++;
                break;
            }
        } else {
            r = next_line(r);
        }
        line++;
    }
    // flush text here!
    if (errors > 0) {
        mem_free(t);
        t = 0;
    }
    return t;
}

void tpl_free(template *t)
{
    mem_free(t);
}

int tpl_execute(template *t, struct buffer *buffer)
{
    return 0;
}

void tpl_test(void)
{
    char tpl[] = {
        "<p>\n"
        "  # -- comment out\n"
        "hello\n"
        "#if q\n"
        "world\n"
        "#end\n"
    };
    template *t = tpl_parse(tpl);
    tpl_free(t);
}
