#include <algorithm>
#include <assert.h>
#include <functional>
#include <iostream>
#include <list>
#include <map>
#include <math.h>
#include <memory>
#include <queue>
#include <sstream>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#define ARRAY_SIZEOF(A) (sizeof(A) / sizeof(A[0]))
#define NON_COPYABLE(C)                                                        \
    C(const C &) = delete;                                                     \
    C &operator=(const C &) = delete;

#define CHECK(E)                                                               \
    do {                                                                       \
        if (!(E)) {                                                            \
            cout << "CHECK failed at " << __FILE__ << "@" << __LINE__ << endl; \
            exit(EXIT_FAILURE);                                                \
        }                                                                      \
    } while (false)

using namespace std;

using i64 = long long int;
using i32 = int;

constexpr char kEol[] = "\n";
constexpr int INF = 0x7fffffff;
constexpr int MOD = 1000000007;

template <class T, class Cmp = std::less<T>>
class MinHeap {
public:
    MinHeap() {}
    explicit MinHeap(std::vector<T> data): h(std::move(data)) {
        std::make_heap(h.begin(), h.end(), cmp);
    }

    void push(T v) {
        h.push_back(v);
        std::push_heap(h.begin(), h.end(), cmp);
    }

    bool empty() const {
        return h.empty();
    }
    T top() const {
        return h[0];
    }

    T pop() {
        std::pop_heap(h.begin(), h.end(), cmp);
        T min = h.back();
        h.pop_back();
        return min;
    }

private:
    struct SwapCmp {
        bool operator()(T a, T b) const {
            return c(b, a);
        }
        Cmp c;
    } cmp;
    std::vector<T> h;
};

struct WinDraw {
    int w = -1, d = -1;

    WinDraw() {}
    WinDraw(int w, int d): w(w), d(d) {}
    int score() const {
        return (w << 1) + d;
    }
    WinDraw inc(int iw, int id) {
        return {w + iw, d + id};
    }

    // Note > here to make descending order.
    bool operator<(const WinDraw &other) const {
        return d > other.d;
    }
};

ostream &operator<<(ostream &os, WinDraw wd) {
    return os << "w=" << wd.w << " d=" << wd.d;
}

class TheTournamentDivOne {
public:
    static constexpr int MAX = 10000;

    int find(const vector<int> &points, int w, int d) {
        fill_win_draws(w, d);
        int nw = 0, nd = 0;
        vector<WinDraw> wd(points.size());
        for (int i = 0; i < points.size(); i++) {
            auto p = points[i];
            if (decomp[p].w < 0) return -1;
            wd[i] = decomp[p];
            nw += wd[i].w;
            nd += wd[i].d;
        }
        if (nd % 2 == 0) return nw + nd / 2;
        auto l = lcm(w, d);
        if ((l / d) % 2 == 0) return -1;
        auto undraw = l / d;
        auto i_max = -1;
        for (int i = 0; i < wd.size(); i++) {
            if (wd[i].d >= undraw && (i_max < 0 || wd[i].d > wd[i_max].d)) {
                i_max = i;
            }
        }
        if (i_max < 0) return -1;
        wd[i_max].w += l / w;
        wd[i_max].d -= undraw;
        nw += l / w;
        nd -= undraw;
        // Check if the solution is valid.
        MinHeap<WinDraw> h(wd);
        auto valid = true;
        while (!h.empty() && h.top().d > 0) {
            auto t = h.pop();
            if (h.empty() || h.top().d == 0) {
                valid = false;
                break;
            }
            auto second = h.pop();
            t.d--;
            second.d--;
            h.push(t);
            h.push(second);
        }
        if (!valid) return -1;
        return nw + nd / 2;
    }

    void fill_win_draws(int w, int d) {
        decomp.resize(MAX + 1);
        decomp[0] = WinDraw(0, 0);
        for (int i = 0; i <= MAX; i++) {
            if (decomp[i].w < 0) continue;
            set_decomp(i + w, decomp[i].inc(1, 0));
            set_decomp(i + d, decomp[i].inc(0, 1));
        }
    }

    void set_decomp(int i, WinDraw val) {
        if (i < decomp.size() &&
            (decomp[i].w < 0 || val.score() < decomp[i].score())) {
            decomp[i] = val;
        }
    }

    static int gcd(int a, int b) {
        while (b != 0) {
            int t = a % b;
            a = b;
            b = t;
        }
        return a;
    }

    static int lcm(int a, int b) {
        return a * b / gcd(a, b);
    }

    vector<WinDraw> decomp;
};

int main(int argc, char **argv) {
    ios_base::sync_with_stdio(false);
    {
        TheTournamentDivOne sol;
        cout << sol.find({10, 1, 1}, 2, 1) << endl;
    }
    {
        TheTournamentDivOne sol;
        cout << sol.find({1, 1, 1}, 2, 1) << endl;
    }
    {
        TheTournamentDivOne sol;
        cout << sol.find({1, 4, 0, 2}, 3, 1) << endl;
    }
    {
        TheTournamentDivOne sol;
        cout << sol.find({8, 3, 8, 5, 9, 2, 7, 11}, 3, 2) << endl;
    }
    return 0;
}
