#include <stdlib.h>
#include <memory.h>
#include <stdio.h>
#include <assert.h>

#define array_of(a) (sizeof(a) / sizeof(a[0]))
#define GEN_MAX 200000

struct op {
    int type;
    int p1, p2;
};

struct gint {
    int n;
    int g;
};

struct gen_int {
    int n;
    int g0, g1;
};

void test_sum()
{
    int xs[] = { 1, 3, 5, 8, 9 };
    int n = array_of(xs);
    int sum1 = 0;
    for (int i = 0; i < n; ++i) {
        for (int j = i + 1; j < n; ++j) {
            sum1 += xs[j] - xs[i];
        }
    }
    printf("sum1 %d\n", sum1);

    int j0 = 0;
    int j1 = array_of(xs) - 1;
    int sum2 = 0;
    while (j0 < j1) {
        int m = j1 - j0;
        printf("m %d j0 %d j1 %d\n", m, j0, j1);
        sum2 += (xs[j1] - xs[j0]) * m;
        j0++;
        j1--;
    }
    printf("sum2 %d\n", sum2);
}

int main(int argc, char **argv)
{
    int n = 0;
    scanf("%d", &n);
    struct gint *xs = malloc(sizeof(struct gint) * n);
    for (int i = 0; i < n; ++i) {
        scanf(" %d", &xs[i].n);
        xs[i].g = 0;
    }
    int m = 0;
    scanf("%d", &m);

    /* Fortunately for both types 1 and 2 we have 2 parameters, so we need
       just 3 ints per operation to read */
    struct op *ops = malloc(sizeof(struct op) * m);
    int move_num = 0, sum_num = 0;
    for (int i = 0; i < m; ++i) {
        struct op *op_i = &oper_list[i];
        scanf(" %d %d %d", &op_i.type, &op_i.p1, &op_i.p2);
        type == 1 ? move_num++ : sum_num++;
    }

    int n_allg = n + move_num;
    struct gen_int *all_num = malloc(sizeof(struct gen_int) * n_allg);
    struct gen_int *gi = all_num;
    for (int i = 0; i < n; ++i) {
        gi->n = xs[i].n;
        gi->g0 = 0;
        gi->g1 = GEN_MAX;
        gi++;
    }
    for (int i = 0; i < m; ++i) {
        if (ops[i].type == 1) {
            int j = ops[i].p1;
            int d = ops[i].p2;
            int g = i + 1;
            xs[j].n += d;
            gi->n = xs[j];
            gi->g0 = xs[j];
        }
    }
    free(xs);

    return 0;
}
