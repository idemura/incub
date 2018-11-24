#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <map>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

using namespace std;

using i32 = int32_t;
using i64 = int64_t;
using pii = std::pair<int, int>;

struct event {
    int c; // Coordinate
    int x;
    int y;
    int left_point;
    int i;
};

struct permutation {
    int8_t p[102];

    void identity() {
        for (int i = 0; i < 102; i++) {
            p[i] = i;
        }
    }

    // Apply @a than @b
    void merge(permutation const &a, permutation const &b) {
        for (int i = 0; i < 102; i++) {
            assert(0 <= a.p[i] && a.p[i] < 102);
            p[i] = b.p[a.p[i]];
        }
    }
};

// Returns even number larger or equal @n
int to_even(int n) {
    if (n == 1) {
        return 1;
    }
    return n + (n & 1);
}

void merge(vector<vector<permutation>> &pm, int i) {
    for (int lvl = 0; lvl + 1 < pm.size(); lvl++) {
        i = min(i, i ^ 1);
        pm[lvl + 1][i / 2].merge(pm[lvl][i], pm[lvl][i ^ 1]);
        i /= 2;
    }
}

int main() {
    int an = 0, qn = 0;
    scanf("%d", &an);
    vector<int> as(an);
    for (auto &a : as) {
        scanf("%d", &a);
    }
    scanf("%d", &qn);
    vector<event> es;
    for (int i = 0; i < qn; i++) {
        event e;
        e.i = i;
        int l, r;
        scanf("%d%d%d%d", &l, &r, &e.x, &e.y);
        e.left_point = 1;
        e.c = l - 1;
        es.push_back(e);
        e.c = r;
        e.left_point = 0;
        es.push_back(e);
    }
    sort(es.begin(), es.end(), [](auto const &a, auto const &b) -> bool {
        if (a.c == b.c) {
            return a.i < b.i;
        } else {
            return a.c < b.c;
        }
    });
    vector<vector<permutation>> pm;
    qn = to_even(qn);
    pm.push_back(vector<permutation>(qn));
    while (pm.back().size() != 1) {
        for (auto &p : pm.back()) {
            p.identity();
        }
        qn = to_even(qn / 2);
        pm.push_back(vector<permutation>(qn));
    }
    assert(pm.back().size() == 1);
    pm.back()[0].identity();
    int j = 0;
    for (int i = 0; i < as.size(); i++) {
        while (j < es.size() && i == es[j].c) {
            auto &p = pm[0][es[j].i];
            if (es[j].left_point) {
                p.p[es[j].x] = es[j].y;
            } else {
                p.p[es[j].x] = es[j].x;
            }
            merge(pm, es[j].i);
            j++;
        }
        printf("%d ", pm.back()[0].p[as[i]]);
    }
    printf("\n");
    return 0;
}
