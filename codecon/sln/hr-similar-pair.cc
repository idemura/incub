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

// Indices are 1..n (inclusive).
template <class Num>
class BIT {
public:
    explicit BIT(int n): v_(n + 1), a_(n + 1) {}
    size_t size() const {
        return a_.size() - 1;
    }

    void add(int i, Num x) {
        a_[i] += x;
        while (i < v_.size()) {
            v_[i] += x;
            i += i & (-i);
        }
    }

    void set(int i, Num x) {
        add(i, x - a_[i]);
    }

    Num get(int i) const {
        return a_[i];
    }

    // Sum of [a..b], inclusive.
    Num sum(int a, int b) const {
        return sum(b) - sum(a - 1);
    }

    // Sum of [0..i], inclusive.
    Num sum(int i) const {
        auto a = Num();
        while (i != 0) {
            a += v_[i];
            i -= i & (-i);
        }
        return a;
    }

    // void print() const {
    //   cout << "BIT: ";
    //   for (int i = 1; i < a_.size(); i++) {
    //     cout << a_[i] << " ";
    //   }
    //   cout << endl;
    // }

private:
    vector<Num> v_, a_;
};

struct DFS {
    DFS(vector<vector<int>> a, int range):
            a(move(a)),
            bit(this->a.size() - 1),
            range(range) {}

    struct Frame {
        int v = 0;
        int i = 0;
        Frame(int v, int i): v(v), i(i) {}
    };

    i64 dfs(int v) {
        vector<Frame> stack{{v, 0}};
        i64 ans = 0;
        while (!stack.empty()) {
            auto f = stack.back();
            if (f.i == 0) {
                auto l = max(1, f.v - range);
                auto r = min((int)bit.size(), f.v + range);
                ans += bit.sum(l, r);
                bit.add(f.v, 1);
            }
            if (f.i < a[f.v].size()) {
                auto w = a[f.v][f.i];
                f.i++;
                stack.back() = f;
                stack.push_back({w, 0});
            } else {
                stack.pop_back();
                bit.add(f.v, -1);
            }
        }
        return ans;
    }

    vector<vector<int>> a;
    BIT<int> bit;
    int range = 0;
};

int main(int argc, char **argv) {
    ios_base::sync_with_stdio(false);
    int n, t;
    cin >> n >> t;
    vector<vector<int>> a(n + 1);
    vector<int> sink(n + 1);
    fori(n - 1) {
        int s, e;
        cin >> s >> e;
        a[s].push_back(e);
        sink[e] = 1;
    }
    int v = 1;
    while (v < sink.size() && sink[v]) {
        v++;
    }
    DFS dfs(move(a), t);
    cout << dfs.dfs(v) << endl;
    return 0;
}
