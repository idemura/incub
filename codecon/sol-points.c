#include <stdlib.h>
#include <memory.h>
#include <stdio.h>
#include <assert.h>
#include <search.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))

#define GEN_MAX 200000

struct op {
    int type;
    int p1, p2;
};

struct gint_ord;
struct gint {
    int n;
    struct gint_ord *gi;
};

struct gint_ord {
    int n;
    int g0, g1;
};

long long sum_array(int *xs, int n)
{
    if (n <= 1) {
        return 0;
    }

    int i = 0;
    int j = n - 1;
    long long sum = 0;
    while (i < j) {
        int m = j - i;
        sum += (xs[j] - xs[i]) * m;
        i++;
        j--;
    }
    return sum;
}

void test_sum_array()
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
    printf("sum2 %d\n", (int)sum_array(xs, n));
}

int gint_ord_compare(struct gint_ord *p1, struct gint_ord *p2)
{
    return p1->n - p2->n;
}

int binary_search(struct gint_ord *arr, int n, int x)
{
    int i = 0;
    int j = n;
    while (i < j) {
        int mid = (i + j) / 2;
        if (x <= arr[mid].n) {
            j = mid;
        } else {
            i = mid + 1;
        }
    }
    return i;
}

void test_binary_search()
{
    struct gint_ord xs[] = { {1}, {3}, {5}, {8}, {9}, {0} };
    int n = array_of(xs) - 1; /* Last 0 is just for print convenience. */
    for (int i = 0; i <= 10; ++i) {
        int j = binary_search(xs, n, i);
        printf("key %d: %d -> %d\n", i, j, xs[j].n);
    }
}

int main(int argc, char **argv)
{
    static int s_buf[100000];
    int g = 0;
    int i, j;

    // test_sum_array();
    // test_binary_search();
    // return 0;

    int n = 0;
    scanf("%d", &n);
    struct gint *xs = malloc(sizeof(struct gint) * n);
    for (i = 0; i < n; ++i) {
        scanf(" %d", &xs[i].n);
        xs[i].gi = 0;
    }
    int m = 0;
    scanf("%d", &m);

    /* Fortunately for both types 1 and 2 we have 2 parameters, so we need
       just 3 ints per operation to read */
    struct op *ops = malloc(sizeof(struct op) * m);
    int move_num = 0;
    for (i = 0; i < m; ++i) {
        struct op *op_i = &ops[i];
        scanf(" %d %d %d", &op_i->type, &op_i->p1, &op_i->p2);
        if (op_i->type == 1) {
            move_num++;
        }
    }

    int n_all = n + move_num;
    struct gint_ord *all_num = malloc(sizeof(struct gint_ord) * n_all);
    struct gint_ord *gi = all_num;
    for (i = 0; i < n; ++i) {
        gi->n = xs[i].n;
        gi->g0 = 0;
        gi->g1 = GEN_MAX;
        xs[i].gi = gi;
        gi++;
    }
    g = 0;
    for (i = 0; i < m; ++i) {
        if (ops[i].type == 1) {
            g++;
            int j = ops[i].p1 - 1;
            int d = ops[i].p2;
            xs[j].n += d;
            xs[j].gi->g1 = g;
            gi->n = xs[j].n;
            gi->g0 = g;
            gi->g1 = GEN_MAX;
            xs[i].gi = gi;
            gi++;
        }
    }
    free(xs);

    /* And now sort them and apply all queries */
    qsort(all_num, n_all, sizeof(*all_num),
        (int (*)(const void*, const void*))gint_ord_compare);

    printf("sorted: ");
    for (i = 0; i < n_all; ++i) {
        printf(" %d[%d %d]", all_num[i].n, all_num[i].g0, all_num[i].g1);
    }
    printf("\n");

    g = 0;
    for (i = 0; i < m; ++i) {
        int l, li, r, ri;
        int k = 0;
        if (ops[i].type == 1) {
            g++;
            continue;
        }

        l = ops[i].p1;
        li = binary_search(all_num, n_all, l);
        r = ops[i].p2;
        ri = binary_search(all_num, n_all, r);
        printf("query sum %d @%d / %d @%d\n", l, li, r, ri);
        /* Since binary search returns less or equal position, move it if `ri`
           has equal match. */
        if (ri < n_all && all_num[ri].n == r) {
            ri++;
        }
        /* Now find amount of active ints int this generation. */
        k = 0;
        printf("g %d\n", g);
        for (j = li; j < ri; ++j) {
            printf("  %d   %d %d\n", all_num[j].n, all_num[j].g0, all_num[j].g1);
            if (all_num[j].g0 <= g && g < all_num[j].g1) {
                s_buf[k++] = all_num[j].n;
            }
        }
        printf("buf: \n");
        for (j = 0; j < k; ++j)
        {
            printf("%d ", s_buf[j]);
        }
        printf("\n");
        long long sum = sum_array(s_buf, k);
        printf("%lld\n", sum); /* Not sure about format */
        printf("---------------------------------------\n");
    }

    free(all_num);
    free(ops);
    return 0;
}
