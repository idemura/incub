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

int n;
vector<int> len;
// Ordered path lengths to subtree node
vector<pii> subtree;
vector<int> ordered_len;
vector<i64> ordered_sum;

pii merge(pii a, int a_edge, pii b, int b_edge) {
    pii res;
    res.first = ordered_len.size();
    ordered_len.push_back(0);
    while (a.first < a.second && b.first < b.second) {
        if (ordered_len[a.first] + a_edge < ordered_len[b.first] + b_edge) {
            ordered_len.push_back(ordered_len[a.first] + a_edge);
            a.first++;
        } else {
            ordered_len.push_back(ordered_len[b.first] + b_edge);
            b.first++;
        }
    }
    while (a.first < a.second) {
        ordered_len.push_back(ordered_len[a.first] + a_edge);
        a.first++;
    }
    while (b.first < b.second) {
        ordered_len.push_back(ordered_len[b.first] + b_edge);
        b.first++;
    }
    res.second = ordered_len.size();
    return res;
}

void rec_merge(int v) {
    if (v >= n) {
        return;
    } else {
        auto il = 2 * v + 1;
        auto ir = 2 * v + 2;
        rec_merge(il);
        rec_merge(ir);
        pii l{0, 0}, r{0, 0};
        int l_edge = 0, r_edge = 0;
        if (il < n) {
            l = subtree[il];
            l_edge = len[il];
        }
        if (ir < n) {
            r = subtree[ir];
            r_edge = len[ir];
        }
        subtree[v] = merge(l, l_edge, r, r_edge);
    }
}

i64 get_node_score(int v, i64 h) {
    auto v_begin = subtree[v].first;
    int j = lower_bound(
                    ordered_len.begin() + v_begin,
                    ordered_len.begin() + subtree[v].second,
                    h) -
            (ordered_len.begin() + v_begin);
    i64 s = h * j - ordered_sum[j + v_begin - 1];
    return s;
}

i64 get_score(int v, i64 h) {
    i64 score = get_node_score(v, h);
    while (v != 0 && h > 0) {
        h -= len[v];
        if (h <= 0) {
            break;
        }
        // Can reach v's parent, add it's score
        score += h;
        v = (v & 1 ? v + 1 : v - 1);
        if (v < n && h > len[v]) {
            score += get_node_score(v, h - len[v]);
        }
        v = (v - 1) / 2;
    }
    return score;
}

int main() {
    int m = 0;
    scanf("%d%d", &n, &m);
    len.resize(n);
    for (int i = 1; i < n; i++) {
        scanf("%d", &len[i]);
    }
    subtree.resize(n);
    rec_merge(0);
    ordered_sum.resize(ordered_len.size());
    for (int i = 0; i < n; i++) {
        i64 s = 0;
        for (int j = subtree[i].first; j < subtree[i].second; j++) {
            s += ordered_len[j];
            ordered_sum[j] = s;
        }
    }
    for (int i = 0; i < m; i++) {
        int a = 0, h = 0;
        scanf("%d%d", &a, &h);
        printf("%lld\n", get_score(a - 1, h));
    }
    return 0;
}
