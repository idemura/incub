#include "base.h"

// Range Minimum query. T is an integral value.
template <class T, T max_value = std::numeric_limits<T>::max()>
class RMQTree {
public:
    explicit RMQTree(const std::vector<T> &a): n(a.size()) {
        tree.resize(2 * n - 1);
        build(0, 0, n, a);
    }

    RMQTree(RMQTree &&other): tree(std::move(other.tree)) {}
    RMQTree(const RMQTree &other) = default;
    RMQTree &operator=(const RMQTree &other) = default;

    // A segment i to j (exclusive).
    int range_min(int i, int j) const {
        return query(0, 0, n, i, j);
    }

    void set(int i, int x) {
        int a = 0, b = n, k = 0;
        for (;;) {
            tree[k] = std::min(tree[k], x);
            if (b - a == 1) break;
            auto l = left_num(b - a);
            if (i < a + l) {
                b = a + l;
                k += 1;
            } else {
                a = a + l;
                k += 2 * l;
            }
        }
    }

private:
    // If we have n leaves, how many should go to the left tree?
    static int left_num(int n) {
        // Split power of two in halves, or take largest power of two.
        return n & (n - 1) ? 1 << static_cast<int>(log2(n)) : n / 2;
    }

    int build(int k, int i, int j, const std::vector<T> &a) {
        if (j - i == 1) {
            tree[k] = a[i];
        } else {
            // So, left part will have 2*l-1 nodes starting k+1 so right will
            // start at k+2*l.
            auto l = left_num(j - i), lk = k + 1, rk = k + 2 * l;
            tree[k] = std::min(build(lk, i, i + l, a), build(rk, i + l, j, a));
        }
        return tree[k];
    }

    int query(int k, int i, int j, int qi, int qj) const {
        if (qj <= i || j <= qi) {
            return max_value;
        }
        if (qi <= i && j <= qj) {
            return tree[k];
        }
        auto l = left_num(j - i), lk = k + 1, rk = k + 2 * l;
        return std::min(
                query(lk, i, i + l, qi, qj), query(rk, i + l, j, qi, qj));
    }

    std::vector<int> tree;
    int n = 0;
};

void check_range(const vector<int> &a, const RMQTree<int> &rmq, int i, int j) {
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
    RMQTree<int> rmq(a);
    check_range(a, rmq, 0, 10);
    check_range(a, rmq, 0, 1);
    check_range(a, rmq, 0, 4);
    check_range(a, rmq, 3, 6);
    check_range(a, rmq, 9, 10);
    check_range(a, rmq, 5, 9);
    check_range(a, rmq, 5, 7);
    check_range(a, rmq, 7, 10);
    cout << "TEST1 passed" << endl;
}

void test2() {
    vector<int> a{1, 3, 2, 8, 9, 4, 5, 5, 7, 1};
    RMQTree<int> rmq(a);
    rmq.set(4, 0);
    a[4] = 0;
    check_range(a, rmq, 0, 10);
    check_range(a, rmq, 0, 1);
    check_range(a, rmq, 0, 4);
    check_range(a, rmq, 3, 6);
    check_range(a, rmq, 3, 5);
    check_range(a, rmq, 4, 5);
    check_range(a, rmq, 5, 10);
    check_range(a, rmq, 5, 7);
    check_range(a, rmq, 7, 10);
    cout << "TEST2 passed" << endl;
}

int main(int argc, char **argv) {
    ios_base::sync_with_stdio(false);
    test1();
    test2();
    return 0;
}
