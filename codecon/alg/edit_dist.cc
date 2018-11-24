#include "base.h"

template <class T>
ostream &operator<<(ostream &os, const vector<T> &v) {
    os << "[";
    if (v.size() >= 1) {
        for (int i = 0; i < v.size(); i++) {
            os << v[i];
            if (i != v.size() - 1) os << " ";
        }
    }
    os << "]";
    return os;
}

// Edit distance of 3 operations: insert, remove, replace of all weight 1.
int edit_dist(const string &a, const string &b) {
    // Only one line of the matrix we compute.
    // 1-based index.
    vector<int> dp1(1 + a.size()), dp2(dp1.size());
    // Init with edit distance from empty string to @b.
    for (int i = 0; i < dp1.size(); i++) {
        dp1[i] = i;
    }
    for (int i = 0; i < b.size(); i++) {
        dp2[0] = i + 1;
        for (int j = 0; j < a.size(); j++) {
            if (b[i] == a[j]) {
                dp2[j + 1] = dp1[j];
            } else {
                dp2[j + 1] = min(dp1[j] + 1, min(dp1[j + 1] + 1, dp2[j] + 1));
            }
        }
        dp1.swap(dp2);
    }
    return dp1[a.size()];
}

// Edit distance of 4 operations: as in edit_dist and transpose.
int edit_dist_tr(const string &a, const string &b) {
    vector<int> dp[3];
    for (int i = 0; i < 3; i++) {
        dp[i].resize(1 + a.size());
    }
    for (int i = 0; i <= a.size(); i++) {
        dp[1][i] = i;
    }
    for (int i = 0; i < b.size(); i++) {
        dp[2][0] = i + 1;
        for (int j = 0; j < a.size(); j++) {
            if (b[i] == a[j]) {
                dp[2][j + 1] = dp[1][j];
            } else {
                dp[2][j + 1] =
                        min(dp[1][j] + 1, min(dp[1][j + 1] + 1, dp[2][j] + 1));
                if (i >= 1 && j >= 1 && a[j] == b[i - 1] && a[j - 1] == b[i]) {
                    dp[2][j + 1] = min(dp[2][j + 1], dp[0][j - 1] + 1);
                }
            }
        }
        dp[0].swap(dp[1]);
        dp[1].swap(dp[2]);
    }
    return dp[1][a.size()];
}

int main() {
    CHECK(0 == edit_dist("", ""));
    CHECK(0 == edit_dist("abc", "abc"));
    CHECK(1 == edit_dist("abcc", "abc"));
    CHECK(1 == edit_dist("zabc", "abc"));
    CHECK(1 == edit_dist("abcz", "abc"));
    CHECK(1 == edit_dist("abc", "abbc"));
    CHECK(1 == edit_dist("abc", "zabc"));
    CHECK(1 == edit_dist("abc", "abcz"));
    CHECK(1 == edit_dist("abc", "agc"));
    CHECK(2 == edit_dist("abcr", "agcd"));
    CHECK(2 == edit_dist("abcr", "agc"));
    CHECK(2 == edit_dist("abc", "agcd"));
    CHECK(2 == edit_dist("fsat", "fast"));
    CHECK(2 == edit_dist("asft", "fast"));

    CHECK(0 == edit_dist_tr("", ""));
    CHECK(0 == edit_dist_tr("abc", "abc"));
    CHECK(1 == edit_dist_tr("abcc", "abc"));
    CHECK(1 == edit_dist_tr("zabc", "abc"));
    CHECK(1 == edit_dist_tr("abcz", "abc"));
    CHECK(1 == edit_dist_tr("abc", "abbc"));
    CHECK(1 == edit_dist_tr("abc", "zabc"));
    CHECK(1 == edit_dist_tr("abc", "abcz"));
    CHECK(1 == edit_dist_tr("abc", "agc"));
    CHECK(2 == edit_dist_tr("abcr", "agcd"));
    CHECK(2 == edit_dist_tr("abcr", "agc"));
    CHECK(2 == edit_dist_tr("abc", "agcd"));
    CHECK(1 == edit_dist_tr("fsat", "fast"));
    CHECK(1 == edit_dist_tr("afst", "fast"));
    // Insert 'f' in the front, remove 'f' in 3rd position.
    CHECK(2 == edit_dist_tr("asft", "fast"));

    cout << "TESTS PASSED." << endl;
    return 0;
}
