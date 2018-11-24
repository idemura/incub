#include <algorithm>
#include <functional>
#include <iostream>
#include <list>
#include <map>
#include <math.h>
#include <random>
#include <sstream>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#define ARRAY_SIZEOF(A) (sizeof(A) / sizeof(A[0]))
#define fori(N) for (int i = 0; i < N; i++)
#define forj(N) for (int j = 0; j < N; j++)

using namespace std;

using i64 = long long int;
using i32 = int;

constexpr char kEol[] = "\n";
constexpr int INF = 0x7fffffff;
constexpr int MOD = 1000000007;
constexpr int DIM = 100002;

struct KthBits {
    // i-th is 2^i-th parent of this vertex, 0 if no such parent.
    int bit[18] = {};
};

void add(vector<KthBits> &kbit, int v, int pred) {
    for (int i = 0; i < 18 && pred != 0; i++) {
        kbit[v].bit[i] = pred;
        pred = kbit[pred].bit[i];
    }
}

int query(const vector<KthBits> &kbit, int v, int k) {
    int b = 0;
    while (v && k) {
        if (k & 1) {
            v = kbit[v].bit[b];
        }
        b++;
        k >>= 1;
    }
    return v;
}

void bfs(const vector<vector<int>> &a, vector<KthBits> &kbit) {
    vector<pair<int, int>> q;
    q.reserve(DIM);
    q.emplace_back(a[0][0], 0);
    for (int i = 0; i < q.size(); i++) {
        auto v = q[i].first;
        add(kbit, v, q[i].second);
        for (auto w : a[v]) {
            q.emplace_back(w, v);
        }
    }
}

void problem() {
    int n, q;
    cin >> n;
    vector<vector<int>> a(DIM);
    for (int i = 0; i < n; i++) {
        int v, w;
        cin >> v >> w;
        a[w].push_back(v);
    }
    vector<KthBits> kbit(DIM);
    bfs(a, kbit);
    cin >> q;
    for (int i = 0; i < q; i++) {
        int t, x, y;
        cin >> t;
        if (t == 0) {
            cin >> y >> x;
            add(kbit, x, y);
        } else if (t == 1) {
            cin >> x;
            kbit[x] = KthBits();
        } else if (t == 2) {
            cin >> x >> y;
            cout << query(kbit, x, y) << endl;
        }
    }
}

int main(int argc, char **argv) {
    ios_base::sync_with_stdio(false);
    int t;
    cin >> t;
    while (t--) {
        problem();
    }
    return 0;
}
