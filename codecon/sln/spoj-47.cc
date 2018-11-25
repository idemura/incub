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

typedef long long int lli;

void extGCD(int a, int b, int *gcd, int *x_out, int *y_out) {
    int x = 0, y = 1;
    int u = 1, v = 0;
    while (a != 0) {
        div_t qr = div(b, a);
        int m = x - u * qr.quot;
        int n = y - v * qr.quot;
        b = a;
        a = qr.rem;
        x = u, y = v;
        u = m, v = n;
    }
    *gcd = b;
    *x_out = x;
    *y_out = y;
}

lli longMulDiv(int a, int b, int d) {
    return (lli)a * (lli)b / d;
}

struct Problem {
    static const int DIM = 100;

    int f, e, a, b;
    int first[DIM], step[DIM];
    int am[DIM][DIM]; // Adjacency matrix.
    int am_n[DIM];
    bool ar[DIM]; // A reachable, B reachable.
    bool br[DIM];
    int mark[DIM], mval;

    Problem():
            f(),
            e(),
            a(),
            b(),
            first(),
            step(),
            am(),
            am_n(),
            ar(),
            br(),
            mark(),
            mval() {}

    void solve() {
        scanf("%d%d%d%d", &f, &e, &a, &b);
        for (int i = 0; i < e; i++) {
            scanf("%d%d", step + i, first + i);
            ar[i] = accessible(i, a);
            br[i] = accessible(i, b);
        }

        bool reachable = canMove();
        printf("%s.\n",
               reachable ? "It is possible to move the furniture"
                         : "The furniture cannot be moved");
    }

    bool canMove() {
        if (a == b) {
            return true;
        }
        // Make adjacency matrix for elevators. Two elevators are connected, if
        // they have some floor where both stop.
        for (int i = 0; i < e; i++) {
            for (int j = i + 1; j < e; j++) {
                if (elevatorsConnected(i, j)) {
                    am[i][am_n[i]++] = j;
                    am[j][am_n[j]++] = i;
                }
            }
        }

        bool reachable = false;
        for (int i = 0; i < e && !reachable; i++) {
            if (ar[i]) {
                mval++;
                reachable = dfs(i);
            }
        }
        return reachable;
    }

    bool accessible(int i, int x) {
        return x >= first[i] && (x - first[i]) % step[i] == 0;
    }

    bool elevatorsConnected(int i, int j) {
        // Fi + Si * Ki = Fj + Sj * Kj => Fi - Fj = Sj * Kj - Si * Ki. So,
        // minimum integer can be represented this way is `g = gcd(Si, Sj)`.
        // Therefore, if `Fi - Fj` divides by `g`, elevators are connected. Have
        // to check if connection is in the limits.
        int g, ki, kj;
        extGCD(step[i], step[j], &g, &ki, &kj);
        int df = first[i] - first[j];
        if (df % g != 0) {
            return false;
        } else {
            int p = df / g;
            // Note for `i` the same will be with `-ki`.
            lli c = first[j] + step[j] * kj * p;
            int max = std::max(first[i], first[j]);
            lli lcm = longMulDiv(step[i], step[j], g);
            if (c < max) {
                c += ((max - c) / lcm + 1) * (int)lcm;
            }
            return c < f;
        }
    }

    // `ni` is node index.
    bool dfs(int ni) {
        if (mark[ni] == mval) {
            return false;
        }
        // We can also exclude from begin points.
        ar[ni] = false;
        mark[ni] = mval;
        if (br[ni]) {
            return true;
        }
        for (int j = 0; j < am_n[ni]; j++) {
            if (dfs(am[ni][j])) {
                return true;
            }
        }
        return false;
    }
};

int main(int argc, char **argv) {
    // #ifndef ONLINE_JUDGE
    //   freopen("in", "r", stdin);
    // #endif
    int t = 0;
    scanf("%d", &t);
    for (; t-- > 0;) {
        Problem *p = new Problem();
        p->solve();
        delete p;
    }
    return 0;
}
