#include <algorithm>
#include <assert.h>
#include <map>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <vector>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define INF 0x7fffffff

typedef long long int lli;

void reverse(char *s, int l, int len) {
    for (int i = 0, j = len; j-- > l; i++) {
        s[j] = s[i];
    }
    s[len] = 0;
}

char *nextPal(char *sn) {
    int i, j;

    for (; *sn == '0' && sn[1] != 0; sn++) {
    }

    int sn_len = strlen(sn);
    if (sn_len == 1) {
        if (sn[0] < '9') {
            sn[0] += 1;
        } else {
            strcpy(sn, "11");
        }
        return sn;
    }

    // Compare reversed first half with second half.
    int cmp = 0;
    int lo = sn_len / 2, hi = (sn_len + 1) / 2;
    i = lo;
    j = hi;
    for (; i--; j++) {
        int c = sn[i] - sn[j];
        if (c != 0) {
            cmp = c > 0 ? 1 : -1;
            break;
        }
    }
    if (cmp > 0) {
        reverse(sn, hi, sn_len);
        return sn;
    }

    // Leave only higher half in the number.
    sn[hi] = 0;
    // Increment this number and shift by one to the right.
    int inc = 1;
    for (i = hi; i-- && inc;) {
        if (sn[i] == '9') {
            sn[i] = '0';
        } else {
            sn[i]++;
            inc = 0;
        }
    }
    sn[hi + 1] = 0;
    if (inc) {
        sn--;
        sn_len++;
        sn[0] = '1';
        hi++;
    }

    // Increment lo and hi since we shifted by 1.
    reverse(sn, hi, sn_len);
    return sn;
}

int main(int argc, char **argv) {
    // #ifndef ONLINE_JUDGE
    //     freopen("in", "r", stdin);
    // #endif
    static char sn[1000008];
    int t;
    scanf("%d", &t);
    char *p = sn + 4;
    for (; t-- > 0;) {
        scanf("%s", p);
        printf("%s\n", nextPal(p));
    }
    return 0;
}
