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

bool smileyBalance(const char *s_in) {
    std::vector<int> balance, sm_open, sm_close;

    for (const char *s = s_in; *s;) {
        if (s[0] == ':' && s[1] == ')') {
            sm_close.push_back(s - s_in);
            s += 2;
        } else if (s[0] == ':' && s[1] == '(') {
            sm_open.push_back(s - s_in);
            s += 2;
        } else if (s[0] == ')') {
            if (balance.size() > 0) {
                balance.pop_back();
            } else if (sm_open.size() > 0) {
                sm_open.pop_back();
            } else {
                return false;
            }
            s++;
        } else if (s[0] == '(') {
            balance.push_back(s - s_in);
            s++;
        } else {
            s++;
        }
    }
    for (; balance.size() > 0 && sm_close.size() > 0;) {
        if (balance.back() < sm_close.back()) {
            balance.pop_back();
            sm_close.pop_back();
        } else {
            return false;
        }
    }
    return balance.size() == 0;
}

char *skipSpaces(char *s) {
    for (; isspace(*s); s++)
        ;
    return s;
}

int main(int argc, char **argv) {
    // #ifndef ONLINE_JUDGE
    //   freopen("in", "r", stdin);
    // #endif
    auto *f = fopen(argv[1], "rt");
    if (!f) {
        return -1;
    }
    char buf[1024] = {};
    for (; fgets(buf, ARRAY_SIZEOF(buf), f);) {
        char *s = skipSpaces(buf);
        if (*s == 0) {
            continue;
        }

        bool balanced = smileyBalance(s);
        printf("%s\n", balanced ? "YES" : "NO");
    }
    fclose(f);
    return 0;
}
