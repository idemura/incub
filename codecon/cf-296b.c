#include <stdlib.h>
#include <memory.h>
#include <stdio.h>
#include <assert.h>

const int MOD = 1000000007;

FILE *open_input(int argc, char **argv);
void close_input(FILE *fin);

int addmod(int x, int y)
{
    return ((long long)x + (long long)y) % MOD;
}

int submod(int x, int y)
{
    return ((long long)x - (long long)y + MOD) % MOD;
}

int mulmod(int x, int y)
{
    return ((long long)x * (long long)y) % MOD;
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

int is_comparable(int n, char *m1, char *m2)
{
    int i;
    int ls = 0;
    int gt = 0;
    for (i = 0; i < n; ++i) {
        ls += m1[i] <= m2[i];
        gt += m1[i] >= m2[i];
    }
    return ls == n || gt == n;
}

int check_step(int n, char *m1, char *m2, int i)
{
    int j, k;
    int acc = 0;
    char *p = 0;

    if (i == n) {
        int c = is_comparable(n, m1, m2);
        /*
        if (c) {
             printf("%s\n%s\n\n", m1, m2);
        }
        */
        return !c;
    }

    if (m1[i] == '?' && m2[i] == '?') {
        for (j = '0'; j <= '9'; ++j) {
            m1[i] = j;
            for (k = '0'; k <= '9'; ++k) {
                m2[i] = k;
                acc += check_step(n, m1, m2, i + 1);
            }
        }
        /* Important to return ? back */
        m1[i] = m2[i] = '?';
        return acc;
    }

    if (m1[i] != '?' && m2[i] != '?') {
        return check_step(n, m1, m2, i + 1);
    }

    p = (m1[i] == '?'? m1: m2) + i;
    for (j = '0'; j <= '9'; ++j) {
        *p = j;
        acc += check_step(n, m1, m2, i + 1);
    }
    /* Important to return ? back */
    *p = '?';

    return acc;
}

int check(int n, char *m1, char *m2)
{
    return check_step(n, m1, m2, 0);
}

int count(int n, char *m1, char *m2)
{
    int i;
    int gt = 1, ls = 1;
    int no_q = 0; /* Need this because mod arithmetics. */
    int comb = 1;
    int q2 = 0;
    int eq = 1;
    int twice = 0;
    int comparable = 0;

    for (i = 0; i < n; ++i) {
        int qa = m1[i] == '?';
        int qb = m2[i] == '?';
        if (qa && qb) {
            q2++;
            ls = mulmod(ls, 55);
            gt = mulmod(gt, 55);
            comb = mulmod(comb, 100);
        } else if (qa || qb) {
            /* ls_i and gt_i are less or equal and greater or equal count. */
            int ls_i = m1[i] == '?' ? m2[i] - '0' : '9' - m1[i];
            int gt_i = ('9' - '0') - ls_i;
            ls_i++;
            gt_i++;
            ls = mulmod(ls, ls_i);
            gt = mulmod(gt, gt_i);
            comb = mulmod(comb, 10);
        } else {
            int diff = m1[i] - m2[i];
            if (diff != 0) {
                eq = 0;
                if (diff < 0) {
                    gt = 0;
                } else {
                    ls = 0;
                }
            }
            no_q++;
        }
    }

    if (no_q == n) {
        /* This is pair without wild card. Answer depends on if this pair
           is comparable itself or not. If `ls` and `gt` are zeros this pair
           is incomparable.
        */
        return ls == 0 && gt == 0 ? 1 : 0;
    }

    /* Equal pair counted twice in some combinations. */
    twice = (q2 ? powmod(10, q2) : 1) * eq;
    comparable = submod(addmod(ls, gt), twice);
    return submod(comb, comparable);
}

int main(int argc, char **argv)
{
    char *m1 = 0;
    char *m2 = 0;
    int n_count = 0;
    int n = 0;
    scanf(" %d", &n);
    if (n <= 0) {
        return -1;
    }
    m1 = malloc(n + 1);
    scanf(" %s", m1);
    m2 = malloc(n + 1);
    scanf(" %s", m2);
    n_count = count(n, m1, m2);
    printf("%d\n", n_count);
    /*
    int n_check = check(n, m1, m2);
    printf("check %d\n", n_check);
    if (n_check == n_count) {
        printf("OK!\n");
    }
    */
    free(m1);
    free(m2);
    return 0;
}
