#include <algorithm>
#include <assert.h>
#include <ctype.h>
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

// Copyable and assignable.
struct SStr {
    char *p;
    int length;

    SStr(): p(), length() {}
    explicit SStr(char *p): p(p), length(strlen(p)) {}

    bool operator<(const SStr &r) const {
        if (length != r.length) {
            return length < r.length;
        }
        const unsigned int *p1 = (const unsigned int *)p;
        const unsigned int *p2 = (const unsigned int *)r.p;
        for (; *p1 == *p2 && *p1; p1++, p2++) {
        }
        return *p1 < *p2;
    }
    bool operator>(const SStr &r) const {
        return r < *this;
    }
};

typedef std::map<SStr, int> SStrMap;

// If word length is 16, we will get 16 words of length 15. So we have:
// 16*15*10000 = 2400*1000 = 2.4Mb. I think 4Mb is enough.
char str_mem[4 << 20];
int str_memp;
int str_mem_size = ARRAY_SIZEOF(str_mem);
int mark[16000];
// `pair[i][j]` How many pairs `(a, b)` at position `i`?
int pair[80][800];

inline int hash2c(const char *p) {
    int p0 = p[0];
    if (p0) p0 -= 'a' - 1;
    int p1 = p[1];
    if (p1) p1 -= 'a' - 1;
    return (p0 << 5) + p1;
}

SStr addString(const char *s) {
    // Check available memory.
    SStr str(strcpy(str_mem + str_memp, s));
    int n = str.length + 1;
    n += 2 * sizeof(int) - n % sizeof(int);
    str_memp += n;
    str_mem_size -= n;
    for (int i = 0;; i++) {
        int h = hash2c(str.p + i);
        pair[i][h] = 1;
        if (!str.p[i]) break;
    }
    return str;
}

int getNetworkSize(SStr in, int vi, const SStrMap &dict, int m) {
    int sum = 0;
    if (mark[vi] == m) {
        return sum;
    }
    mark[vi] = m;

    char buf[80] = {}, t = 0;
    SStr str(buf);
    const int n = in.length;
    SStrMap::const_iterator end_it = dict.end();

    // Replace
    strcpy(buf, in.p);
    str.length = n;
    for (int i = 0; i < n; i++) {
        t = buf[i];
        for (int c = 'a'; c <= 'z'; c++) {
            buf[i] = c;
            if (pair[i][hash2c(buf + i)]) {
                SStrMap::const_iterator it = dict.find(str);
                if (it != end_it) {
                    sum += getNetworkSize(it->first, it->second, dict, m);
                }
            }
        }
        buf[i] = t;
    }

    // Remove
    strcpy(buf, in.p);
    str.length = n - 1;
    t = 0;
    for (int i = n - 1;; i--) {
        char buf_i = buf[i];
        buf[i] = t;
        if (pair[i][hash2c(buf + i)]) {
            SStrMap::const_iterator it = dict.find(str);
            if (it != end_it) {
                sum += getNetworkSize(it->first, it->second, dict, m);
            }
        }
        if (i == 0) break;
        t = buf_i;
    }

    // Insert
    strcpy(buf, in.p);
    str.length = n + 1;
    for (int i = n;; i--) {
        if (pair[i + 1][hash2c(buf + i + 1)]) {
            for (int c = 'a'; c <= 'z'; c++) {
                buf[i] = c;
                SStrMap::const_iterator it = dict.find(str);
                if (it != end_it) {
                    sum += getNetworkSize(it->first, it->second, dict, m);
                }
            }
        }
        if (i == 0) break;
        buf[i] = buf[i - 1];
    }

    return sum + 1;
}

int main(int argc, char **argv) {
    // #ifndef ONLINE_JUDGE
    //   freopen("in", "r", stdin);
    // #endif

    FILE *f = fopen(argv[1], "rt");
    if (!f) {
        return -1;
    }

    std::vector<SStr> ws;
    std::map<SStr, int> dict;
    bool input_end = false;
    char buf[80] = {};
    for (int i = 0; fgets(buf, ARRAY_SIZEOF(buf), f); i++) {
        // Trim right (at least, new lines).
        int n = strlen(buf);
        for (; n > 0 && isspace(buf[n - 1]); n--) {
        }
        buf[n] = 0;
        if (buf[0] == 0) {
            continue; // Empty line.
        }
        if (strcmp(buf, "END OF INPUT") == 0) {
            input_end = true;
            continue;
        }
        SStr s = addString(buf);
        if (!input_end) {
            ws.push_back(s);
        } else {
            dict[s] = i;
        }
    }
    fclose(f);

    for (int i = 0; i < ws.size(); i++) {
        int ns = getNetworkSize(ws[i], 0, dict, i + 1) - 1;
        printf("%d\n", ns);
    }
    return 0;
}
