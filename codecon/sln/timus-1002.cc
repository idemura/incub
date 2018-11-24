#include <algorithm>
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <utility>
#include <vector>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define INF 0x7fffffff
#define MOD 1000000007

using namespace std;

#define MAX 108

char encode(char c) {
    switch (c) {
    case 'i':
    case 'j':
        return '1';
    case 'a':
    case 'b':
    case 'c':
        return '2';
    case 'd':
    case 'e':
    case 'f':
        return '3';
    case 'g':
    case 'h':
        return '4';
    case 'k':
    case 'l':
        return '5';
    case 'm':
    case 'n':
        return '6';
    case 'p':
    case 'r':
    case 's':
        return '7';
    case 't':
    case 'u':
    case 'v':
        return '8';
    case 'w':
    case 'x':
    case 'y':
        return '9';
    case 'o':
    case 'q':
    case 'z':
        return '0';
    default:
        return 0;
    }
}

struct dp_t {
    int w, c;
};

vector<int> ix[MAX];
int wlen[50000];
dp_t dp[MAX];
char sample[MAX];
int sample_l;
char dict[50000][60];
int dict_n;

void dpf() {
    int i, j, k, c;
    for (i = sample_l; i--;) {
        int c_min = INF, j_min = -1;
        for (j = 0; j < ix[i].size(); j++) {
            k = i + wlen[ix[i][j]];
            if (k >= sample_l) {
                c = 0;
            } else {
                c = dp[k].c;
            }
            if (c < c_min) {
                c_min = c;
                j_min = j;
            }
        }
        dp[i].c = c_min == INF ? INF : c_min + 1;
        dp[i].w = j_min < 0 ? -1 : ix[i][j_min];
    }
}

int main() {
#ifndef ONLINE_JUDGE
    freopen("in", "r", stdin);
#endif
    int i, j, k;

    for (;;) {
        scanf("%s", sample);
        if (strcmp(sample, "-1") == 0) {
            break;
        }
        sample_l = strlen(sample);
        for (i = 0; i < MAX; i++) {
            dp[i].w = dp[i].c = -1;
            ix[i].clear();
        }
        scanf("%d", &dict_n);
        for (i = 0; i < dict_n; i++) {
            char e[60], *d = dict[i];
            scanf("%s", d);
            wlen[i] = strlen(d);
            for (j = 0; d[j]; j++) {
                e[j] = encode(d[j]);
            }
            e[j] = 0;
            char *p = strstr(sample, e);
            while (p) {
                k = p - sample;
                if (k + wlen[i] <= sample_l) {
                    ix[k].push_back(i);
                }
                p = strstr(p + 1, e);
            }
        }
        dpf();

        int c = dp[0].c, w = dp[0].w;
        if (c == INF) {
            printf("No solution.\n");
        } else {
            i = 0;
            do {
                printf("%s ", dict[w]);
                i += wlen[w];
                w = dp[i].w;
                c--;
            } while (c > 0);
            printf("\n");
        }
    }
    return 0;
}
