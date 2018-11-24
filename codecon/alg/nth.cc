#include "base.h"

void print(const vector<int> &a, int i, int j) {
    for (auto x : a)
        cout << x << " ";
    cout << endl;
    for (int k = 0; k < a.size(); k++) {
        if (k == i && i == j) {
            cout << "i,j";
        } else if (k == i) {
            cout << "i ";
        } else if (k == j) {
            cout << "j ";
        } else {
            cout << "  ";
        }
    }
    cout << endl;
}

// If p is a return value, then partitions are [i, p] and [p+1, j].
// Requires elements of `a` to be less comparable.
template <class T, class Cmp = std::less<T>>
int random_partition(std::vector<T> &a, int i, int j, Cmp cmp = Cmp()) {
    if (i == j) return -1;
    auto i_in = i;
    auto j_in = j;
    auto pivot_i = i + rand() % (j - i + 1);
    // Put pivot in the end to avoid bounds check in the first while and to have
    // at least one in the right part.
    if (pivot_i != j) std::swap(a[pivot_i], a[j]);
    auto p = a[j];
    for (;;) {
        // Don't need bounds check because the last is element of the range is
        // pivot.
        while (cmp(a[i], p))
            i++;
        // Use <= because we want to leave last element on its place.
        while (i < j && !cmp(a[j], p))
            j--;
        if (i >= j) break;
        std::swap(a[i], a[j]);
        i++;
        j--;
    }
    if (i == i_in) {
        // Pivot was the minimal element.
        std::swap(a[j], a[j_in]);
        return i;
    } else {
        return i - 1;
    }
}

// Returns the value of ith element.
template <class T, class Cmp = std::less<T>>
int nth(vector<T> &a, int ith, Cmp cmp = Cmp()) {
    CHECK(ith < a.size());
    int i = 0;
    int j = a.size() - 1;
    while (i != j) {
        auto p = random_partition(a, i, j, cmp);
        if (ith <= p) {
            j = p;
        } else {
            i = p + 1;
        }
    }
    return a[i];
}

void test1() {
    srand(1);
    vector<int> a{5, 8, 3, 7, 2, 4, 6, 9, 1, 0};
    for (int i = 0; i < a.size(); i++) {
        CHECK(nth(a, i) == i);
    }
    cout << "test1 OK\n";
}

void test2() {
    srand(1);
    vector<int> a{5, 5, 5, 5, 5};
    for (int i = 0; i < a.size(); i++) {
        CHECK(nth(a, i) == 5);
    }
    cout << "test2 OK\n";
}

void test3() {
    srand(1);
    vector<int> a{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
    for (int i = 0; i < a.size(); i++) {
        CHECK(nth(a, i) == i);
    }
    cout << "test3 OK\n";
}

void test4() {
    srand(1);
    vector<int> a{5, 8, 3, 7, 1, 7, 6, 9, 0, 0}, b;
    CHECK(nth(b = a, 0) == 0);
    CHECK(nth(b = a, 1) == 0);
    CHECK(nth(b = a, 2) == 1);
    CHECK(nth(b = a, 3) == 3);
    CHECK(nth(b = a, 4) == 5);
    CHECK(nth(b = a, 5) == 6);
    CHECK(nth(b = a, 6) == 7);
    CHECK(nth(b = a, 7) == 7);
    CHECK(nth(b = a, 8) == 8);
    CHECK(nth(b = a, 9) == 9);
    cout << "test4 OK\n";
}

int main() {
    test1();
    test2();
    test3();
    test4();
    cout << "TESTS PASSED." << endl;
    return 0;
}
