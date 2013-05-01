#define _GNU_SOURCE
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <search.h>
#include <stdio.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define SIZE_MAX 48

typedef void *rbtree_t;
typedef long long int lli;

int mini(int a, int b) {
    return a < b? a: b;
}
int maxi(int a, int b) {
    return a > b? a: b;
}

typedef struct {
    int n;
    int l[SIZE_MAX];
} alist;

void dfs(int v, alist *al, int *visit, alist *trace)
{
    int i;

    if (visit[v]) {
        return;
    }
    visit[v] = 1;
    trace->l[trace->n++] = v;
    for (i = 0; i < al[v].n; ++i) {
        dfs(al[v].l[i], al, visit, trace);
    }
}

void linked_cmpts(alist *al, int n, alist *lc, int *lc_n)
{
    int visit[SIZE_MAX];
    int i;

    memset(visit, 0, sizeof visit);
    for (i = 0; i < n; ++i) {
        if (!visit[i]) {
            dfs(i, al, visit, lc + *lc_n);
            (*lc_n)++;
        }
    }
}

void split_on_groups(alist *lc, int lc_n)
{
    /* groups (linked components) of 3, 2 and 1 items in */
    alist g[3];
    int i;

    g[0].n = g[1].n = g[2].n = 0;
    for (i = 0; i < lc_n; ++i) {
        if (lc[i].n > 3) {
            printf("-1\n");
            return;
        } else {
            alist *al = &g[lc[i].n - 1];
            al->l[al->n++] = i;
        }
    }

    if (g[1].n > g[0].n) {
        printf("-1\n");
        return;
    }

    for (i = 0; i < g[2].n; ++i) {
        alist *al3 = &lc[g[2].l[i]];
        printf("%d %d %d\n", al3->l[0] + 1, al3->l[1] + 1, al3->l[2] + 1);
    }

    for (i = 0; i < g[1].n; ++i) {
        alist *al2 = &lc[g[1].l[i]];
        alist *al1 = &lc[g[0].l[i]];
        printf("%d %d %d\n", al2->l[0] + 1, al2->l[1] + 1, al1->l[0] + 1);
    }

    for (; i < g[0].n; i += 3) {
        printf("%d %d %d\n",
            lc[g[0].l[i]].l[0] + 1,
            lc[g[0].l[i + 1]].l[0] + 1,
            lc[g[0].l[i + 2]].l[0] + 1);
    }
}

int main(int argc, char **argv)
{
    int al_n = 0, lc_n = 0, m = 0, i, j;
    alist al[SIZE_MAX]; /* adjacency list of initial graph */
    alist lc[SIZE_MAX]; /* linked components */

    scanf("%d%d", &al_n, &m);
    for (i = 0; i < al_n; ++i) {
        al[i].n = 0;
    }

    for (i = 0; i < m; ++i) {
        int a, b;
        scanf("%d%d", &a, &b);
        /* 1-based indices in file. */
        a--;
        b--;
        alist *al_a = al + a;
        al_a->l[al_a->n++] = b;
    }

    linked_cmpts(al, al_n, lc, &lc_n);
    /*
    printf("linked components:\n");
    for (i = 0; i < lc_n; ++i) {
        alist *ali = lc + i;
        printf("%d: ", i);
        for (j = 0; j < ali->n; ++j) {
            printf("%d ", ali->l[j]);
        }
        printf("\n");
    }
    */
    split_on_groups(lc, lc_n);
    return 0;
}
