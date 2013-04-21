#include <stdlib.h>
#include <memory.h>
#include <stdio.h>
#include <assert.h>

const int MOD = 1000000007;

#define DIG0 '0'
#define DIG9 '9'

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

inline static int mulmod(int x, int y)
{
    long long xy = (long long)x * (long long)y;
    if (xy >= MOD) {
        return (int)(xy % MOD);
    } else {
        return (int)xy;
    }
}

int powmod(int x, int p)
{
    int acc = 1;
    for (; p; p >>= 1) {
        if (p & 1) {
            acc = mulmod(acc, x);
        }
        x = mulmod(x, x);
    }
    return acc;
}

int mulsum_mod(int a1, int a2, int b1, int b2)
{
    long long a12 = (long long)a1 * (long long)a2;
    long long b12 = (long long)b1 * (long long)b2;
    long long sum = a12 + b12;
    return (int)(sum % MOD);
}

int combinations(int n)
{
    return powmod(DIG9 - DIG0 + 1, n);
}

int is_comparable(int n, char *m1, char *m2)
{
    int ls = 0;
    int gt = 0;
    for (int i = 0; i < n; ++i) {
        ls += m1[i] <= m2[i];
        gt += m1[i] >= m2[i];
    }
    return ls == n || gt == n;
}

int check_step(int n, char *m1, char *m2, int i)
{
    if (i == n) {
        int c = is_comparable(n, m1, m2);
        // if (c) {
        //     printf("%s\n%s\n\n", m1, m2);
        // }
        return !c;
    }

    if (m1[i] == '?' && m2[i] == '?') {
        printf("Two ? in pattern, exit\n");
        exit(-1);
    }
    if (m1[i] != '?' && m2[i] != '?') {
        return check_step(n, m1, m2, i + 1);
    }

    char *p = (m1[i] == '?'? m1: m2) + i;
    int acc = 0;
    for (int j = DIG0; j <= DIG9; ++j) {
        *p = j;
        acc += check_step(n, m1, m2, i + 1);
    }
    // Important to return -1 back
    *p = '?';
    return acc;
}

int check(int n, char *m1, char *m2)
{
    return check_step(n, m1, m2, 0);
}

int count(int n, char *m1, char *m2)
{
    printf("m1 %s\nm2 %s\n", m1, m2);
    // Classify first.
    int gt_num = 0, ls_num = 0, eq_num = 0, q1 = 0, q2 = 0;
    for (int i = 0; i < n; ++i) {
        int qa = m1[i] == '?';
        int qb = m2[i] == '?';
        if (qa && qb) {
            q2++;
        } else if (qa || qb) {
            q1++;
        } else {
            int diff = m1[i] - m2[i];
            if (diff == 0) {
                eq_num++;
            } else {
                diff < 0? ls_num++: gt_num++;
            }
        }
    }

    printf("ls_num %d gt_num %d eq_num %d\n", ls_num, gt_num, eq_num);
    printf("q1 %d q2 %d\n", q1, q2);

    if (ls_num + gt_num + eq_num == n) {
        if (ls_num && gt_num) {
            return 1;
        } else {
            return 0;
        }
    }

    if (q2 != 0) {
        printf("q2 should be 0.\n");
        return -1;
    }

    // Ok, move one ? pair to the beginning.
    for (int i = 0, w = 0; i < n; ++i) {
        int qa = m1[i] == '?';
        int qb = m2[i] == '?';
        if (qa != qb) {
            m1[w] = m1[i];
            m2[w] = m2[i];
            w++;
        }
    }

    int *any = malloc(sizeof(*any) * q1);
    any[q1 - 1] = 1;
    for (int i = q1 - 1; i--;) {
        any[i] = mulmod(any[i + 1], 10);
    }

    printf("any [");
    for (int i = 0; i < q1; ++i) {
        printf(" %d", any[i]);
    }
    printf(" ]\n");

    // So, first `q1` are digit-? pairs (or vice versa).
    int gt = 1, ls = 1;
    for (int i = 0; i < q1; ++i) {
        int ls_i;
        if (m1[i] == '?') {
            ls_i = m2[i] - DIG0;
        } else {
            ls_i = DIG9 - m1[i];
        }
        int gt_i = (DIG9 - DIG0) - ls_i;
        // ls_i and gt_i are less or equal and greater or equal count.
        ls_i++;
        gt_i++;

        ls = mulmod(ls, ls_i);
        gt = mulmod(gt, gt_i);
    }
    printf("ls %d gt %d\n", ls, gt);

    // Subtract 1 because fully equal pair is both counted in `ls` and `gt`.
    int comparable = ls + gt - 1;
    return combinations(n) - comparable;
}

int main(int argc, char **argv)
{
        FILE *fin = get_input(argc, argv);
    int n;
    fscanf(fin, " %d", &n);
    if (n <= 0) {
        return -1;
    }
    char *m1 = malloc(n + 1);
    fscanf(fin, " %s", m1);
    char *m2 = malloc(n + 1);
    fscanf(fin, " %s", m2);
    int n_count = count(n, m1, m2);
    printf("%d\n", n_count);
    int n_check = check(n, m1, m2);
    printf("\ncheck %d\n", n_check);
    if (n_check == n_count) {
        printf("OK!\n");
    }
    free(m1);
    free(m2);
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
