#include <stdlib.h>
#include <memory.h>
#include <stdio.h>

const int MOD = 1000000007;

typedef struct {
    char a, b;
} pair;

FILE *get_input(int argc, char **argv);
void close_input(FILE *fin);

/*
    partition:
    first pairs digit-digit
    then digit-? or ?-digit
    and finally ? - ?

    number of uncamparable for ?-?
    pairs that less are eq to pairs that gt:
    first, exclude equal pairs: 100-10=90
    so, half os gt and half is less: 45!
    number of any for ?-?: 100

    now for every digit 0-9 we have gt and less counts:
    0: less 0 gt 9
    1: less 1 gt 8 so sum is 9 on every row
    ...

    5 ? ?
    ? 4 ?

    0   0   1(5)    1(prev)+1 -- do I really need this row?
    le  0   5       5*4
    gt  0   4       4*(9 - 4)
    2   0   0       + (5(prev le)*(9 - 4) + 4(prev gt)*4) * (don't care pairs)

    We can coutn don't care values in accumulator and update it on next step.
    we don't want to rearange. so, when we meet digit-digit pair, we compare and
    koefs are like 0, 1

    Particulary
    3 5
    4 6

    le  1   1*1
    gt  1   1*0
    2   0   1*0 + 1*1
*/

int modmul(int x, int y)
{
    return ((long long)x * y) % MOD;
}

int count(pair *str, int n)
{
    // Classify first.
    // `wc` stands for wildcard.
    int gt_num = 0, ls_num = 0, eq_num = 0, wc_num = 0, one_q = 0;
    for (int i = 0; i < n; ++i) {
        int wca = str[i].a == '?';
        int wcb = str[i].b == '?';
        if (wca && wcb) {
            wc_num++;
        } else if (!wca && !wcb) {
            int diff = str[i].a - str[i].b;
            if (diff < 0)
                ls_num++;
            else if (diff > 0)
                gt_num++;
            else
                eq_num++;
        } else {
            one_q++;
        }
    }
    if (ls_num + gt_num + eq_num == n) {
        if (ls_num && gt_num) {
            return 1;
        } else {
            return 0;
        }
    }

    // Ok, move one ? pair to the beginning.
    pair *read = str, *write = str;
    for (int i = 0; i < n; ++i) {
        int wca = str[i].a == '?';
        int wcb = str[i].b == '?';
        if (wca != wcb) {
            *write++ = *read++;
        } else {
            read++;
        }
    }

    // So, first `one_ex` are digit-? pairs (or vice versa).
    int gt = 1, ls = 1, total = 0;
    int ls_prev = 0;
    int gt_prev = 0;
    for (int i = 0; i < one_q; ++i) {
        int ls_i;
        if (str[i].a == '?') {
            ls_i = str[i].b - '0';
        } else {
            ls_i = '9' - str[i].a;
        }
        int gt_i = 9 - ls_i;
        if (ls_i)
            ls = modmul(ls, ls_i);
        if (gt_i)
            gt = modmul(gt, gt_i);
        total += (ls_prev * gt_i + gt_prev * ls_i) * _any_;
        ls_prev = ls;
        gt_prev = gt;
    }

    // And now we should count ?-? pairs.
    for (int i = 0; i < wc_num; ++i) {
        ls = modmul(ls, 45);
        gt = modmul(gt, 45);
    }

    return total;
}

int main(int argc, char **argv)
{
    FILE *fin = get_input(argc, argv);
    int n;
    fscanf(fin, " %d", &n);
    if (n <= 0) {
        return -1;
    }
    pair *str = malloc(sizeof(*str) * n);
    char *buf = malloc(n + 1);
    fscanf(fin, " %s", buf);
    for (int i = 0; i < n; ++i) {
        str[i].a = buf[i];
    }
    fscanf(fin, " %s", t2);
    for (int i = 0; i < n; ++i) {
        str[i].b = buf[i];
    }
    free(buf);
    int answer = count(str, n);
    printf("%d\n", answer);
    free(str);
    close_input(fin);
    return 0;
}

FILE *get_input(int argc, char **argv)
{
    for (int i = 0; i < argc; ++i) {
        if (strcmp(argv[i], "-f") == 0) {
            if (i + 1 < argc) {
                FILE *f = fopen(argv[i + 1], "rt");
                if (f) {
                    return f;
                }
            }
            break;
        }
    }
    return stdin;
}

void close_input(FILE *fin)
{
    if (fin && fin != stdin) {
        fclose(fin);
    }
}
