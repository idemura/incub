#include "base.h"

// Finds best split of the array on two halves such that their sum closest to
// half sum of the array.
// https://en.wikipedia.org/wiki/Partition_problem

template <class T>
void print_vec(const vector<T> &a) {
    cout << "[ ";
    for (auto x : a)
        cout << x << " ";
    cout << "]" << endl;
}

i64 sum(const vector<int> &a) {
    i64 s = 0;
    for (auto &x : a) {
        s += x;
    }
    return s;
}

i64 sum_mask(const vector<int> &a, int m, vector<int> &out) {
    out.clear();
    i64 s = 0;
    for (int i = 0; i < a.size(); i++) {
        if (m & 1) {
            s += a[i];
            out.push_back(a[i]);
        }
        m >>= 1;
    }
    return s;
}

int nearest_half_naive(const vector<int> &a, set<vector<int>> &res) {
    CHECK(a.size() <= 16);
    res.clear();
    // If odd sum, then truncate to lower number and this may mask results
    // truncated to higher value.
    const int full_sum = sum(a);
    int best = 0, best_ofs = abs(full_sum - best);
    vector<int> t;
    for (int i = 0, n = 1 << a.size(); i < n; i++) {
        int s = 2 * sum_mask(a, i, t);
        int s_ofs = abs(full_sum - s);
        if (s_ofs <= best_ofs) {
            if (s_ofs < best_ofs) {
                res.clear();
                best = s;
                best_ofs = s_ofs;
            }
            res.insert(move(t));
        }
    }
    return best_ofs;
}

int nearest_half(const vector<int> &a) {
    const int s = sum(a);
    vector<bool> r(s / 2 + 1); // Bit vector is enough.
    r[0] = true;
    int rmax = 0;
    int ofs_min = s;
    // Reallocs if inside the loop below.
    vector<int> new_ix;
    for (auto e : a) {
        int ofs = INF;
        new_ix.clear();
        for (int j = 0; j <= rmax; j++) {
            if (not r[j]) continue;
            int k = j + e;
            if (abs(2 * k - s) < ofs) {
                ofs = abs(2 * k - s);
                ofs_min = min(ofs_min, ofs);
                new_ix.push_back(k);
            } else {
                break;
            }
        }
        // If we do it inside the loop above, we will count same element several
        // times.
        if (new_ix.size() > 0) {
            if (new_ix.back() + 1 > r.size()) {
                r.resize(new_ix.back() + 1);
            }
            for (auto k : new_ix) {
                r[k] = true;
                rmax = max(rmax, k);
            }
        }
    }
    return ofs_min;
}

void print_results(const vector<int> &a) {
    set<vector<int>> res;
    int h = nearest_half_naive(a, res);
    cout << "Closest to half: " << h << endl;
    for (auto &v : res)
        print_vec(v);
    CHECK(h == nearest_half(a));
}

void test() {
    print_results({1, 1, 1, 1});
    print_results({1, 1, 1});
    print_results({1, 1, 2});
    print_results({1, 3, 8, 3, 3});
    print_results({1, 5, 8, 3, 3});
    print_results({10, 20, 30, 5, 40, 50, 40, 15});
}

int main(int argc, char **argv) {
    ios_base::sync_with_stdio(false);
    test();
    return 0;
}
