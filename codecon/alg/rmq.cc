#include "base.h"

// Range Minimum query. T is less comparable.
template <class T>
class RMQ {
public:
    explicit RMQ(std::vector<T> a) {
        int n = a.size(); // We move a so a.size() will be 0.
        t.resize(ilog2(n) + 1);
        t[0] = std::move(a);
        for (int j = 1; j < t.size(); j++) {
            int step = 1 << (j - 1);
            for (int i = 0; i <= n - (1 << j); i++) {
                t[j].push_back(std::min(t[j - 1][i], t[j - 1][i + step]));
            }
        }
    }

    RMQ(RMQ &&other): t(std::move(other.t)) {}
    RMQ(const RMQ &other) = default;
    RMQ &operator=(const RMQ &other) = default;

    // A segment i to j (exclusive).
    int range_min(int i, int j) const {
        int p = ilog2(j - i);
        return std::min(t[p][i], t[p][j - (1 << p)]);
    }

private:
    static int ilog2(int n) {
        return static_cast<int>(log2(n));
    }

    std::vector<std::vector<T>> t;
};

void check_range(const vector<int> &a, const RMQ<int> &rmq, int i, int j) {
    int m = INF;
    for (int k = i; k < j; k++) {
        m = std::min(m, a[k]);
    }
    int rmq_res = rmq.range_min(i, j);
    if (rmq_res != m) {
        cout << "RMQ " << rmq_res << " != " << m << " in " << i << ", " << j
             << endl;
        exit(EXIT_FAILURE);
    }
}

void test1() {
    vector<int> a{1, 3, 2, 8, 9, 4, 5, 5, 7, 1};
    RMQ<int> rmq(a);
    check_range(a, rmq, 0, 10);
    check_range(a, rmq, 0, 1);
    check_range(a, rmq, 0, 4);
    check_range(a, rmq, 3, 6);
    check_range(a, rmq, 9, 10);
    check_range(a, rmq, 5, 9);
    check_range(a, rmq, 5, 7);
    cout << "TEST1 passed" << endl;
}

int main(int argc, char **argv) {
    ios_base::sync_with_stdio(false);
    test1();
    return 0;
}
