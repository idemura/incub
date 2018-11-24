#include "base.h"

template <class T>
class SegmentFn {
public:
    SegmentFn(function<T(T, T)> binary_fn, vector<int> a): fn(move(binary_fn)) {
        if (a.empty()) return;
        auto n = 1;
        for (int i = 2; i <= a.size(); i *= 2) {
            n++;
        }
        cascade.resize(n);
        cascade[0] = move(a);
        for (int i = 1; i < cascade.size(); i++) {
            const auto &prev = cascade[i - 1];
            auto &next = cascade[i];
            next.resize(prev.size() / 2);
            for (int j = 1; j < prev.size(); j += 2) {
                next[j / 2] = fn(prev[j - 1], prev[j]);
            }
        }
    }

    int get_value_on(int il, int ir) const {
        auto r = T();
        auto step = 1;
        auto j = 0;
        // Increase phase.
        // Invariant: @il % @step == 0 and step fits.
        while (ir - il >= step) {
            if ((il & step) != 0) {
                r = fn(r, cascade[j][il / step]);
                il += step;
            }
            step *= 2;
            j++;
        }
        // Decrease phase.
        // Invariant: @il % @step == 0 and step may not fit.
        while (ir - il > 0) {
            if (ir - il >= step) {
                r = fn(r, cascade[j][il / step]);
                il += step;
            }
            step /= 2;
            j--;
        }
        return r;
    }

    const function<T(T, T)> fn;
    vector<vector<T>> cascade;
};

function<int(int, int)> sum_fn() {
    return [](int a, int b) { return a + b; };
}

function<int(int, int)> max_fn() {
    return [](int a, int b) { return std::max(a, b); };
}

int naive(
        const function<int(int, int)> &fn,
        const vector<int> &a,
        int il,
        int ir) {
    int r = 0;
    for (int i = il; i < ir; i++) {
        r = fn(r, a[i]);
    }
    return r;
}

void test1() {
    const vector<int> a{1, 3, 1, 2, 3, 4, 2, 1, 3, 2};
    SegmentFn<int> sf(sum_fn(), a);
    for (int il = 0; il < a.size(); il++) {
        for (int ir = il; ir < a.size(); ir++) {
            auto r = sf.get_value_on(il, ir);
            auto e = naive(sf.fn, a, il, ir);
            if (e != r) {
                cerr << "TEST FAIL: Expected " << e << " actual " << r << " on "
                     << il << ", " << ir << endl;
                CHECK(false);
            }
        }
    }
}

void test2() {
    const vector<int> a{1, 3, 1, 2, 3, 4, 2, 1, 3, 2};
    SegmentFn<int> sf(max_fn(), a);
    for (int il = 0; il < a.size(); il++) {
        for (int ir = il; ir < a.size(); ir++) {
            auto r = sf.get_value_on(il, ir);
            auto e = naive(sf.fn, a, il, ir);
            if (e != r) {
                cerr << "TEST FAIL: Expected " << e << " actual " << r << " on "
                     << il << ", " << ir << endl;
                CHECK(false);
            }
        }
    }
}

int main() {
    test1();
    test2();
    cout << "TESTS PASSED." << endl;
    return 0;
}
