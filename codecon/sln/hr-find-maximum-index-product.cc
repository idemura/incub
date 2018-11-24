#include <algorithm>
#include <functional>
#include <iostream>
#include <list>
#include <map>
#include <math.h>
#include <memory>
#include <queue>
#include <sstream>
#include <stdlib.h>
#include <string>
#include <unordered_map>
#include <unordered_set>
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
class Heap {
public:
    Heap() {}
    explicit Heap(std::vector<T> data): h(std::move(data)) {
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

struct ValueCmp {
    bool operator()(pair<int, int> a, pair<int, int> b) const {
        return a.first < b.first;
    }
};

vector<int> nearest_gt(const vector<int> &a) {
    vector<int> ix(a.size());
    Heap<pair<int, int>, ValueCmp> h;
    for (int i = 0; i < a.size(); i++) {
        while (!h.empty() && h.top().first < a[i]) {
            ix[h.top().second] = i + 1;
            h.pop();
        }
        h.push({a[i], i});
    }
    return ix;
}

int main() {
    ios_base::sync_with_stdio(false);
    int n = 0;
    cin >> n;
    vector<int> a(n);
    for (int i = 0; i < n; i++) {
        cin >> a[i];
    }
    auto r = nearest_gt(a);
    reverse(a.begin(), a.end());
    auto l = nearest_gt(a);
    reverse(l.begin(), l.end());
    for (auto &x : l) {
        if (x) x = n + 1 - x;
    }
    i64 m = 0;
    for (int i = 0; i < n; i++) {
        m = max(m, i64(r[i]) * i64(l[i]));
    }
    cout << m << endl;
    return 0;
}
