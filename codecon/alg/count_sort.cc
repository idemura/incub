#include "base.h"

using namespace std;

void count_sort(std::vector<int> &a) {
    if (a.empty()) return;

    auto max_a = *std::max_element(a.begin(), a.end());
    std::vector<int> c(max_a + 1);
    for (int i = 0; i < a.size(); i++) {
        c[a[i]]++;
    }
    for (int i = 1; i < c.size(); i++) {
        c[i] += c[i - 1];
    }
    for (int i = 0, j = 0; i < c.size(); i++) {
        // CLR recommends to put value backwards to make stable.
        for (; j < c[i]; j++) {
            a[j] = i;
        }
    }
}

bool is_sorted(const vector<int> &a) {
    for (int i = 1; i < a.size(); i++) {
        if (a[i - 1] > a[i]) return false;
    }
    return true;
}

int main() {
    vector<int> a1{5, 8, 3, 7, 2, 4, 6, 9, 1, 0};
    count_sort(a1);
    CHECK(is_sorted(a1));

    vector<int> a2{5, 5, 5, 7, 2, 4, 6, 6, 1, 0};
    count_sort(a2);
    CHECK(is_sorted(a2));

    cout << "TESTS PASSED." << endl;
    return 0;
}
