#include "base.h"

using namespace std;

void print_vector(const vector<int> &a) {
    if (a.size() == 0) {
        cout << "[]";
    } else {
        cout << "[" << a[0];
        for (int i = 1; i < a.size(); i++) {
            cout << " " << a[i];
        }
        cout << "]";
    }
    cout << endl;
}

bool restore_seq(vector<int> s, vector<int> &a) {
    const auto s_size = s.size();
    a.push_back(s[0] + s[1] - s[2]); // 2*a[0] actually.
    if (a[0] % 2 != 0) {
        a.clear();
        return false;
    }
    a[0] /= 2;
    while (true) {
        a.push_back(s[0] - a[0]);
        for (int j = 0, n = a.size() - 1; j < n; j++) {
            auto p = find(s.begin(), s.end(), a[n] + a[j]);
            if (s.end() == p) {
                a.clear();
                return false;
            }
            s.erase(p);
        }
        auto n = a.size() * (a.size() - 1) / 2;
        if (s_size == n) {
            break;
        } else if (n > s.size()) {
            a.clear();
            return false;
        }
    }
    return true;
}

vector<int> restore(vector<int> s) {
    sort(s.begin(), s.end());
    if (s.size() <= 2) {
        if (s.size() <= 1) return s;
        return {};
    }
    // First two are a[0]+a[1] and a[0]+a[2]. s[2] is either a[0]+a[3] or
    // a[1]+a[2]. Try s[2] as a[1]+a[2] and then s[3] if failed. Let's
    // a[0] = s[0] + s[1] - s[2,3]. Restore sequence: erase sums we know.
    // Minimal element is a[0]+a[j].
    vector<int> a;
    if (!restore_seq(s, a)) {
        swap(s[2], s[3]);
        if (!restore_seq(s, a)) {
            return {};
        }
    }
    return a;
}

void print_result(const vector<int> &a) {
    cout << "answer: ";
    if (a.empty()) {
        cout << "invalid" << endl;
    } else {
        for (auto n : a)
            cout << n << " ";
        cout << endl;
    }
}

vector<int> pair_sums(const vector<int> &a) {
    print_vector(a);
    vector<int> s;
    for (int i = 0; i < a.size(); i++) {
        for (int j = 0; j < i; j++) {
            s.push_back(a[i] + a[j]);
        }
    }
    print_vector(s);
    return s;
}

int main() {
    print_result(restore(pair_sums(vector<int>{1, 5, 6, 7})));
    print_result(restore(pair_sums(vector<int>{1, 5, 10, 100})));
    return 0;
}
