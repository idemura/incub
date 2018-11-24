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

int kmp(const string &s, const string &needle) {
    auto pf = prefix_fn(needle);
    int i = 0, j = 0;
    while (i < s.size() && j < needle.size()) {
        if (s[i] == needle[j]) {
            i++;
            j++;
        } else {
            if (j == 0) {
                i++;
            } else {
                j = pf[j];
            }
        }
    }
    return j == needle.size() ? i - needle.size() : (int)string::npos;
}

void test(const string &s, const string &needle) {
    int x = kmp(s, needle);
    int y = s.find(needle);
    if (x != y) {
        cout << "FAILED:\ns=" << s << "\nneedle=" << needle << "\n";
        cout << "  KMP " << x << " find " << y << endl;
    }
}

int main(int argc, char **argv) {
    ios_base::sync_with_stdio(false);
    test("abc abcdab abcdabcdabde", "abcdabd");
    test("abcabcabaad", "aad");
    test("abcabcabaad", "abc");
    test("abcabababcd", "ababc");
    test("abcabcabaad", "cab");
    test("abcabcabaad", "bcab");
    test("abcabcabaad", "aad");
    test("abcabcabaad", "add");
    cout << "TESTS PASSED." << endl;
    return 0;
}
