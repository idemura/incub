#include "base.h"

vector<int> prefix_fn(const string &s) {
    vector<int> pf(s.size());
    // j is length of matching prefix up to (not including) char i.
    for (int i = 1, j = 0; i < s.size();) {
        if (s[i] == s[j]) {
            pf[i] = j;
            j++;
            i++;
        } else {
            if (!pf[i]) pf[i] = j; // Set one(first) time.
            if (j == 0) {
                i++;
            } else {
                j = pf[j];
            }
        }
    }
    return pf;
}

void print_prefix_fn(const string &s, const vector<int> &pf) {
    for (int i = 0; i < s.size(); i++) {
        cout << s << "\n";
        auto p = string(i - pf[i], ' ') + s.substr(0, pf[i]) + string("^\n");
        cout << p;
    }
}

void test(const string &s) {
    auto pf = prefix_fn(s);
    print_prefix_fn(s, pf);
}

int main(int argc, char **argv) {
    ios_base::sync_with_stdio(false);
    test("ababba");
    test("aaaa");
    return 0;
}
