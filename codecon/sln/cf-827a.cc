#include <algorithm>
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <utility>
#include <vector>

using namespace std;

using i32 = int32_t;
using i64 = int64_t;
using pii = std::pair<int, int>;

constexpr size_t kmax = 2'000'000 + 4;

struct str_piece {
    char *p{};
    int pos{};
    int len{};

    str_piece() {}
    str_piece(char *p, int pos, int len): p(p), pos(pos), len(len) {}
};

char buf[kmax];
char res[kmax];

int main() {
    memset(res, 'a', kmax);
    int n = 0;
    scanf("%d", &n);
    vector<str_piece> words;
    auto p = buf;
    int res_len = 0;
    for (int i = 0; i < n; i++) {
        int k = 0;
        scanf("%s%d", p, &k);
        int len = strlen(p);
        for (int j = 0; j < k; j++) {
            int pos = 0;
            scanf("%d", &pos);
            pos--;
            words.emplace_back(p, pos, len);
            res_len = max(res_len, pos + len);
        }
        p += len;
    }
    *p = 0;
    res[res_len] = 0;
    sort(words.begin(), words.end(), [](auto l, auto r) {
        return l.pos < r.pos;
    });
    int j = 0;
    for (auto w : words) {
        int last = w.pos + w.len;
        if (last > j) {
            int first = max(j, w.pos);
            memcpy(res + first, w.p + w.len - (last - first), last - first);
            j = last;
        }
    }
    printf("%s\n", res);
    return 0;
}
