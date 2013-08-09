#include <algorithm>
#include <vector>
#include <utility>
#include <stdio.h>
#include <string.h>
#include <math.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define INF 0x7fffffff
#define MOD 1000000007

using namespace std;

#define MAX 108

char encode(char c)
{
    switch (c) {
    case 'i': case 'j':
        return '1';
    case 'a': case 'b': case 'c':
        return '2';
    case 'd': case 'e': case 'f':
        return '3';
    case 'g': case 'h':
        return '4';
    case 'k': case 'l':
        return '5';
    case 'm': case 'n':
        return '6';
    case 'p': case 'r': case 's':
        return '7';
    case 't': case 'u': case 'v':
        return '8';
    case 'w': case 'x': case 'y':
        return '9';
    case 'o': case 'q': case 'z':
        return '0';
    default:
        return 0;
    }
}

vector<int> ix[100];
int wlen[50000];
int dp[100];

int dpf(int k) {
    int i, j;
    int best = 0;
    for (i = 0; i < ix[k].size(); i++) {
        int d = dpf()
    }
}

int main()
{
#ifndef ONLINE_JUDGE
    freopen("in", "r", stdin);
#endif
    int i, j;
    char sample[MAX];
    int dict_n = 0;

    scanf("%s%d", sample, &dict_n);
    for (i = 0; i < dict_n; i++) {
        char w[60];
        scanf("%s", w);
        for (j = 0; w[j]; j++) {
            w[j] = encode(w[j]);
        }
        char *p = strstr(sample, w);
        while (p) {
            j = p - sample + 1;
            ix[j].push_back(i);
            p = strstr(sample + j, w);
        }
    }

    for (i = 0; i < ARRAY_SIZEOF(dp); i++) {
        dp[i] = -1;
    }
    int sol = dpf(0);

    return 0;
}
