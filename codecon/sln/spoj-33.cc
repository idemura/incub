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

const int DIM = 92;
const int HASH_MOD = 7789;

typedef long long int lli;

// This string has manual memory control to have cheap copy.
// Idea: store string in trie and code them just with int.
struct String {
    static std::vector<char *> str_mem;

    char *s;
    int length;
    int hash;

    String(): s(new char[DIM]), length(), hash() {
        str_mem.push_back(s);
    }

    void append(char c) {
        s[length++] = c;
        s[length] = 0;
        hash = ((hash << 8) + c) % HASH_MOD;
    }

    void freeMem() {
        delete[] s;
        s = NULL;
        length = 0;
    }

    String clone() const {
        String cl;
        strcpy(cl.s, s);
        cl.length = length;
        cl.hash = hash;
        return cl;
    }

    static void freeAllMem() {
        for (int i = 0; i < str_mem.size(); i++) {
            delete[] str_mem[i];
        }
        str_mem.clear();
    }

    int hashCompareTo(const String &str) const {
        if (hash == str.hash) {
            return strcmp(s, str.s);
        } else {
            return hash - str.hash;
        }
    }
};

struct TextOrder {
    bool operator()(const String &lh, const String &rh) {
        return strcmp(lh.s, rh.s) < 0;
    }
};

struct HashOrder {
    bool operator()(const String &lh, const String &rh) {
        return lh.hashCompareTo(rh) < 0;
    }
};
std::vector<char *> String::str_mem;

struct LcsRoute {
    std::vector<String> rs;
    int length;

    LcsRoute(): length() {}

    void clear() {
        length = 0;
        rs.clear();
    }

    void merge(const LcsRoute &v1, const LcsRoute &v2) {
        length = std::max(v1.length, v2.length);
        if (v1.length == v2.length) {
            int i = 0, j = 0;
            for (; i < v1.rs.size() && j < v2.rs.size();) {
                int order = v1.rs[i].hashCompareTo(v2.rs[j]);
                if (order <= 0) {
                    rs.push_back(v1.rs[i]);
                    i++;
                    if (order == 0) {
                        j++;
                    }
                } else {
                    rs.push_back(v2.rs[j]);
                    j++;
                }
            }
            for (; i < v1.rs.size(); i++) {
                rs.push_back(v1.rs[i]);
            }
            for (; j < v2.rs.size(); j++) {
                rs.push_back(v2.rs[j]);
            }
        } else {
            // We could avoid copying, but 1000 possibly is not that big.
            if (v1.length == length) {
                rs = v1.rs;
            } else {
                rs = v2.rs;
            }
        }
    }

    void append(const LcsRoute &lr, char c) {
        length = lr.length + 1;
        if (lr.rs.size() == 0) {
            rs.push_back(String());
            rs[0].append(c);
        } else {
            for (int i = 0; i < lr.rs.size(); i++) {
                rs.push_back(lr.rs[i].clone());
                rs[i].append(c);
            }
            std::sort(rs.begin(), rs.end(), HashOrder());
        }
    }

    void sort() {
        std::sort(rs.begin(), rs.end(), TextOrder());
    }
};

// Two rows of LCS matrix.
LcsRoute lcs(char *a, char *b) {
    // Store only two lines of memoization matrix.
    LcsRoute rv[2][DIM];
    int an = strlen(a);
    int bn = strlen(b);
    int f = 0;
    for (int i = 1; i <= an; i++) {
        for (int j = 1; j <= bn; j++) {
            rv[1 - f][j].clear();
            if (a[i - 1] == b[j - 1]) {
                rv[1 - f][j].append(rv[f][j - 1], a[i - 1]);
            } else {
                rv[1 - f][j].merge(rv[1 - f][j - 1], rv[f][j]);
            }
        }
        f = 1 - f;
    }
    rv[f][bn].sort();
    return rv[f][bn];
}

char a[84], b[84];

void routes() {
    const LcsRoute &lr = lcs(a, b);
    // printf("%d\n", r.length);
    for (int i = 0; i < lr.rs.size(); i++) {
        printf("%s\n", lr.rs[i].s);
    }
    String::freeAllMem();
}

int main(int argc, char **argv) {
    // #ifndef ONLINE_JUDGE
    //   freopen("in", "r", stdin);
    // #endif
    int t = 0;
    scanf("%d", &t);
    for (; t-- > 0;) {
        scanf("%s%s", a, b);
        routes();
    }
    return 0;
}
