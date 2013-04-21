#include <stdlib.h>
#include <memory.h>
#include <stdio.h>
#include <assert.h>

const int MOD = 1000000007;

FILE *get_input(int argc, char **argv);
void close_input(FILE *fin);

int addmod(int x, int y)
{
    long long z = (long long)x + (long long)y;
    if (z >= MOD) {
        return (int)(z % MOD);
    } else {
        return (int)z;
    }
}

int submod(int x, int y)
{
    long long z = (long long)x - (long long)y + MOD;
    if (z >= MOD) {
        return (int)(z % MOD);
    } else {
        return (int)z;
    }
}

int mulmod(int x, int y)
{
    long long z = (long long)x * (long long)y;
    if (z >= MOD) {
        return (int)(z % MOD);
    } else {
        return (int)z;
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
    return powmod('9' - '0' + 1, n);
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
    for (int j = '0'; j <= '9'; ++j) {
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

    int gt = 1, ls = 1;
    int no_q = 0; // Need this because mod arithmetics.
    int comb = 1;
    for (int i = 0; i < n; ++i) {
        int qa = m1[i] == '?';
        int qb = m2[i] == '?';
        if (qa && qb) {
            ls = mulmod(ls, 55);
            gt = mulmod(gt, 55);
            comb = mulmod(comb, 100);
        } else if (qa || qb) {
            // ls_i and gt_i are less or equal and greater or equal count.
            int ls_i = (m1[i] == '?' ? m2[i] - '0' : '9' - m1[i]) + 1;
            int gt_i = ('9' - '0') - ls_i + 1;
            ls = mulmod(ls, ls_i);
            gt = mulmod(gt, gt_i);
            comb = mulmod(comb, 10);
        } else {
            no_q++;
            int diff = m1[i] - m2[i];
            if (diff != 0) {
                if (diff < 0) {
                    gt = 0;
                } else {
                    ls = 0;
                }
            }
        }
    }

    if (no_q == n) {
        // This is pair without wildcard. Answer depends on if this pair
        // is comparable itself or not. If `ls` and `gt` are zeroes this pair
        // is incomparable.
        return ls == 0 && gt == 0 ? 1 : 0;
    }

    // Equal pair counted wtice if we have both less and greater combinations.
    int eq_twice = ls && gt;

    // Subtract 1 because fully equal pair is both counted in `ls` and `gt`.
    int comparable = sub_mod(addmod(ls, gt), eq_twice);
    return modsub(comb, comparable);
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
