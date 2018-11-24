#include <algorithm>
#include <assert.h>
#include <map>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <utility>
#include <vector>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define INF 0x7fffffff

using namespace std;

typedef long long int lli;

class FloodRelief {
public:
    struct Link {
        Link *next, *prev;
        int i, j;
        int h;

        Link(): next(), prev(), i(), j(), h() {}
    };

    struct LinkSort {
        bool operator()(const Link &l, const Link &r) {
            return l.h < r.h;
        }
    };

    struct Cell {
        Link *link;
        int h;
        int m; // Visited mark.

        Cell(): link(), h(), m() {}
    };

    Cell cells[50][50];
    int cr, cc;
    Link pq[50 * 50];
    int pq_n;
    Link *head;
    int mark;

    FloodRelief(): cells(), cr(), cc(), pq(), pq_n(), head(), mark() {}

    int minimumPumps(const vector<string> &heights) {
        cr = heights.size();
        cc = heights[0].size();
        for (int i = 0; i < cr; i++) {
            for (int j = 0; j < cc; j++) {
                pq[pq_n].h = heights[i][j] - 'a';
                pq[pq_n].i = i;
                pq[pq_n].j = j;
                pq_n++;
            }
        }
        // Sort PQ by height.
        sort(pq, pq + pq_n, LinkSort());
        // Link sorted list.
        for (int i = 1; i < pq_n; i++) {
            pq[i - 1].next = &pq[i];
            pq[i].prev = &pq[i - 1];
        }
        head = pq;
        // Link cell to PQ.
        for (int i = 0; i < pq_n; i++) {
            cells[pq[i].i][pq[i].j].link = pq + i;
            cells[pq[i].i][pq[i].j].h = pq[i].h;
        }

        int c = 0;
        while (pq_n > 0) {
            bfs(head);
            c++;
        }
        return c;
    }

    void bfs(Link *first) {
        vector<Link *> level;
        level.push_back(first);
        visit(first);
        for (int i = 0; i < level.size();) {
            int n = level.size();
            for (; i < n; i++) {
                Link *l = level[i];
                adjacents(l, &level);
            }
        }
    }

    void removeFromPQ(Link *l) {
        if (l == head) {
            head = l->next;
        }
        Link *p = l->prev, *n = l->next;
        if (n) {
            n->prev = p;
        }
        if (p) {
            p->next = n;
        }
        l->prev = l->next = NULL;
        pq_n--;
    }

    void adjacents(Link *l, vector<Link *> *level) {
        int i = l->i, j = l->j;
        if (i > 0 && !cells[i - 1][j].m && cells[i][j].h <= cells[i - 1][j].h) {
            level->push_back(cells[i - 1][j].link);
            visit(cells[i - 1][j].link);
        }
        if (j > 0 && !cells[i][j - 1].m && cells[i][j].h <= cells[i][j - 1].h) {
            level->push_back(cells[i][j - 1].link);
            visit(cells[i][j - 1].link);
        }
        if (i + 1 < cr && !cells[i + 1][j].m &&
            cells[i][j].h <= cells[i + 1][j].h) {
            level->push_back(cells[i + 1][j].link);
            visit(cells[i + 1][j].link);
        }
        if (j + 1 < cc && !cells[i][j + 1].m &&
            cells[i][j].h <= cells[i][j + 1].h) {
            level->push_back(cells[i][j + 1].link);
            visit(cells[i][j + 1].link);
        }
    }

    void visit(Link *l) {
        removeFromPQ(l);
        mark++;
        cells[l->i][l->j].m = mark;
    }
};
