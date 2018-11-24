#include <algorithm>
#include <assert.h>
#include <ctype.h>
#include <limits.h>
#include <map>
#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <vector>

#define ARRAY_SIZEOF(A) (sizeof(A) / sizeof(A[0]))
#define INF 0x7fffffff
#define NON_COPYABLE(C)                                                        \
    C(const C &);                                                              \
    C &operator=(const C &);

using namespace std;

typedef long long int lli;

struct ReadBuf {
    static const int buf_alloc_size = 192 * 1024;

    FILE *f;
    char *buf;
    int buf_len;
    int buf_pos;

    ReadBuf(FILE *f):
            f(f),
            buf(new char[buf_alloc_size + 1]),
            buf_len(),
            buf_pos() {
        buf[buf_alloc_size] = 0;
    }

    ~ReadBuf() {
        delete[] buf;
    }

    void readStringToEoln(string *str) {
        str->clear();
        int k = 0;
        while (fillBufferAndSkipSpaces(k == 0)) {
            k++;
            int i = buf_pos;
            while (i < buf_len && buf[i] != '\n') {
                i++;
            }
            str->append(buf + buf_pos, i - buf_pos);
            buf_pos = i;
            if (i < buf_len) break;
        }
    }

    void readInt(int *n) {
        *n = 0;
        int sign = 0;
        int k = 0;
        while (fillBufferAndSkipSpaces(k == 0)) {
            k++;
            int i = buf_pos;
            if (!sign && i < buf_len) {
                if (buf[i] == '-') {
                    sign = -1;
                    i++;
                } else {
                    sign = 1;
                    if (buf[i] == '+') i++;
                }
            }
            while (i < buf_len && '0' <= buf[i] && buf[i] <= '9') {
                *n = *n * 10 + (buf[i] - '0');
                i++;
            }
            buf_pos = i;
            if (i < buf_len) break;
        }
        *n *= sign;
    }

    bool fillBufferAndSkipSpaces(bool skip_spaces) {
        if (buf_pos == buf_len) {
            if (!readToBuffer()) {
                return false; // EOF reached.
            }
        }
        if (skip_spaces) {
            int i = buf_pos;
            while (i < buf_len && isspace(buf[i])) {
                i++;
            }
            buf_pos = i;
        }
        return true;
    }

    bool readToBuffer() {
        if (feof(f)) {
            return false;
        }
        buf_len = fread(buf, 1, buf_alloc_size, f);
        buf_pos = 0;
        return true;
    }

    NON_COPYABLE(ReadBuf);
};

struct Node {
    // Count of open right parens on the left end, and open left parens on the
    // right end.
    int r, l;
    Node(): r(), l() {}
};
typedef vector<Node> Bit;

Node fromChar(char c) {
    Node pb;
    pb.r = c == ')' ? 1 : 0;
    pb.l = c == '(' ? 1 : 0;
    return pb;
}

void init(Bit &bit, int leaf_n) {
    bit.clear();
    int n = leaf_n;
    int s = 0;
    while (true) {
        s += n;
        if (n == 1) break;
        n = (n + 1) / 2;
    }
    bit.resize(s);
}

void print(const Bit &bit, int leaf_n) {
    int j = 0;
    int n = leaf_n;
    while (true) {
        for (int i = 0; i < n; i++) {
            printf("r %d l %d, ", bit[j].r, bit[j].l);
            j++;
        }
        printf("\n");
        if (n == 1) break;
        n = (n + 1) / 2;
    }
}

Node sum(const Node &lh, const Node &rh) {
    Node r;
    int m = std::min(lh.l, rh.r);
    r.l = lh.l + rh.l - m;
    r.r = lh.r + rh.r - m;
    return r;
}

void create(Bit &bit, const string &str) {
    int leaf_n = str.size();
    init(bit, leaf_n);
    for (int i = 0; i < leaf_n; i++) {
        Node pb = fromChar(str[i]);
        bit[i] = pb;
        int n = leaf_n;
        int s = 0;
        int j = i;
        while (n > 1) {
            int up_j = s + n + j / 2;
            bit[up_j] = sum(bit[up_j], pb);
            j /= 2;
            s += n;
            n = (n + 1) / 2;
        }
    }
}

bool checkString(const string &str) {
    int open_left = 0;
    for (int i = 0; i < str.size(); i++) {
        if (str[i] == '(') {
            open_left++;
            continue;
        }
        if (str[i] == ')') {
            if (open_left == 0) return false;
            open_left--;
            continue;
        }
    }
    return open_left == 0;
}

bool check(const Bit &bit) {
    Node pb = bit[bit.size() - 1];
    return pb.r == 0 && pb.l == 0;
}

void update(Bit &bit, string &str, int i) {
    int leaf_n = str.size();
    str[i] = str[i] == '(' ? ')' : '(';
    bit[i] = fromChar(str[i]);
    int n = leaf_n;
    int s = 0;
    int j = i;
    while (n > 1) {
        int up_j = s + n + j / 2;
        if (j & 1) {
            bit[up_j] = sum(bit[s + j - 1], bit[s + j]);
        } else {
            bit[up_j] = sum(bit[s + j], j + 1 < n ? bit[s + j + 1] : Node());
        }
        j /= 2;
        s += n;
        n = (n + 1) / 2;
    }
}

int main(int argc, char **argv) {
#ifndef ONLINE_JUDGE
    freopen("in", "r", stdin);
#endif

    ReadBuf rb(stdin);
    string str;
    Bit bit;
    for (int i = 1; i <= 10; i++) {
        printf("Test %d:\n", i);
        int n = 0, m = 0, op = 0;
        rb.readInt(&n);
        rb.readStringToEoln(&str);

        create(bit, str);
        rb.readInt(&m);
        for (int j = 0; j < m; j++) {
            rb.readInt(&op);
            if (op == 0) {
                bool valid = check(bit);
                // bool valid_check = checkString(str);
                // assert(valid == valid_check);
                printf("%s\n", valid ? "YES" : "NO");
            } else {
                if (1 <= op && op <= str.size()) {
                    update(bit, str, op - 1);
                }
            }
        }
    }
    return 0;
}
