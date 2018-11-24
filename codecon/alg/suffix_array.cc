#include "base.h"

// Don't output if no error.
static bool output_suffix_array = false;

int str_lcp(const string &a, const string &b) {
    auto min_n = min(a.size(), b.size());
    for (int i = 0; i < min_n; i++) {
        if (a[i] != b[i]) return i;
    }
    return min_n;
}

vector<int> suffix_array_naive(const string &s) {
    vector<int> sa;
    for (int i = 0; i < s.size(); i++) {
        sa.push_back(i);
    }
    sort(sa.begin(), sa.end(), [&s](int a, int b) {
        return s.compare(a, string::npos, s, b, string::npos) < 0;
    });
    return sa;
}

struct SuffixPair {
    int f, s; // Order of first and second part of the suffix.
    int i; // Suffix index.
    void set(int i, int f, int s) {
        this->i = i;
        this->f = f;
        this->s = s;
    }
    bool operator<(const SuffixPair &r) const {
        return f < r.f || (f == r.f && s < r.s);
    }
};

void pairs_to_ord(vector<SuffixPair> &sp, vector<int> &ord) {
    sort(sp.begin(), sp.end());
    ord[sp[0].i] = 0;
    for (int i = 1; i < sp.size(); i++) {
        ord[sp[i].i] = ord[sp[i - 1].i] + (sp[i - 1] < sp[i]);
    }
}

// `s` should be pow-of-2 .
// O(n log^2 n).
vector<int> suffix_array(const string &s) {
    const int n = s.size();
    vector<int> ord(n);
    vector<SuffixPair> sp(n);
    for (int i = 0; i < n; i++)
        sp[i].set(i, s[i], 0);
    pairs_to_ord(sp, ord);
    // Not less or equal (<=), because in case of == we compare (effectively)
    // strings of size n made of two part of length l (which is n/2). There is
    // effectively only one of size n - s itself.
    for (int l = 1; 2 * l < n; l = 2 * l) {
        for (int i = 0; i < n; i++) {
            sp[i].f = ord[i];
            sp[i].s = i + l < n ? ord[i + l] : 0;
            sp[i].i = i;
        }
        pairs_to_ord(sp, ord);
    }

    vector<int> count(n), sa(n);
    for (int i = 0; i < n; i++) {
        count[ord[i]]++;
    }
    for (int i = 1; i < n; i++) {
        count[i] += count[i - 1];
    }
    for (int i = 0; i < n; i++) {
        sa[--count[ord[i]]] = i;
    }
    return sa;
}

// See comments in suffix_array.
vector<int> suffix_array_ord(const string &s, vector<vector<int>> &ord) {
    const int n = s.size();
    ord.resize(log2(n));
    for (auto &v : ord)
        v.resize(n);
    vector<SuffixPair> sp(n);
    for (int i = 0; i < n; i++)
        sp[i].set(i, s[i], 0);
    pairs_to_ord(sp, ord[0]);
    int k = 0;
    for (int l = 1; 2 * l < n; l = 2 * l) {
        for (int i = 0; i < n; i++) {
            sp[i].f = ord[k][i];
            sp[i].s = i + l < n ? ord[k][i + l] : 0;
            sp[i].i = i;
        }
        pairs_to_ord(sp, ord[++k]);
    }

    vector<int> count(n), sa(n);
    for (int i = 0; i < n; i++) {
        count[ord[k][i]]++;
    }
    for (int i = 1; i < n; i++) {
        count[i] += count[i - 1];
    }
    for (int i = 0; i < n; i++) {
        sa[--count[ord[k][i]]] = i;
    }
    return sa;
}

// Longest Common Prefix of two suffixes.
// O(log n).
int suffix_array_lcp(const vector<vector<int>> &ord, int i, int j) {
    // Tricky moment: because a and _ (symbol after string's end) and both
    // zeros, we may mistakingly get LCP of 'b_' and 'ba' is 2 instead of 1. To
    // avoid this, clamp to the max string size.
    int max_lcp = ord[0].size() - max(i, j);
    if (i == j) return max_lcp;
    int lcp = 0;
    int k = ord.size() - 1;
    for (; k >= 0; k--) {
        // We have 2^k in common? If yes, move.
        if (ord[k][i] == ord[k][j]) {
            lcp += 1 << k;
            i += 1 << k;
            j += 1 << k;
        }
    }
    return min(lcp, max_lcp);
}

void test_suffix_array(string s) {
    auto printer = [&s](const vector<int> &sa) {
        for (auto i : sa) {
            cout << s.substr(i) << endl;
        }
    };

    const int n = s.size();
    cout << "input: " << s << endl;
    auto sa_n = suffix_array_naive(s);
    auto sa_y = suffix_array(s);
    vector<vector<int>> ord;
    auto sa_x = suffix_array_ord(s, ord);
    if (sa_x != sa_y) {
        cout << "suffix_array and suffix_array_ord - different results!\n";
        return;
    }
    if (sa_n != sa_x) {
        cout << "ERROR!\n";
        cout << "naive:\n";
        printer(sa_n);
        cout << "advanced algo:\n";
        printer(sa_x);
    } else {
        if (output_suffix_array) {
            cout << "suffix array:\n";
            printer(sa_x);
        }
    }

    // Check LCPs.
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            auto b1 = str_lcp(s.substr(i), s.substr(j));
            auto b2 = suffix_array_lcp(ord, i, j);
            if (b1 != b2) {
                cout << "ERROR lcp i=" << i << " j=" << j << ": " << b1
                     << " vs " << b2 << "\n";
                cout << s.substr(i) << endl;
                cout << s.substr(j) << endl;
            }
        }
    }
}

int main(int argc, char **argv) {
    ios_base::sync_with_stdio(false);
    test_suffix_array("ba");
    test_suffix_array("bacb");
    test_suffix_array("bacbacab");
    test_suffix_array("baacdcab");
    test_suffix_array("aaaa");
    test_suffix_array("dbcaaccb");
    return 0;
}
