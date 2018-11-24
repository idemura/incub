#include <bits/stdc++.h>

using namespace std;

int get_max(const vector<int> &a, int except) {
    int m = -1;
    for (int i = 0; i < a.size(); i++) {
        if (i == except) continue;
        if (m < 0 || a[i] > a[m]) {
            m = i;
        }
    }
    return m;
}

bool all_same(const vector<int> &a) {
    for (int i = 1; i < a.size(); i++) {
        if (a[i - 1] != a[i]) {
            return false;
        }
    }
    return true;
}

void get_max_rating(const vector<int> &a, vector<int> &ix) {
    ix.clear();
    for (int i = 0; i < a.size(); i++) {
        if (ix.empty() || a[i] > a[ix[0]]) {
            ix.clear();
            ix.push_back(i);
        } else if (a[i] == a[ix[0]]) {
            ix.push_back(i);
        }
    }
}

void print_move_log(int n, vector<int> &move_log) {
    for (int i = 0; i < move_log.size();) {
        string s(n, '0');
        int step_n = move_log[i++];
        for (; step_n > 0; step_n--) {
            s[move_log[i++]] = '1';
        }
        cout << s << "\n";
    }
}

int main() {
    int n;
    cin >> n;
    vector<int> r(n);
    for (int i = 0; i < n; i++) {
        cin >> r[i];
    }
    int moves = 0;
    vector<int> move_log;
    for (;;) {
        int m = get_max(r, -1);
        int next_to_max = get_max(r, m);
        if (r[m] == r[next_to_max]) {
            break;
        }
        moves++;
        move_log.push_back(2);
        move_log.push_back(m);
        move_log.push_back(next_to_max);
        r[m]--;
        if (r[next_to_max] > 0) {
            r[next_to_max]--;
        }
    }
    vector<int> ix;
    while (!all_same(r)) {
        get_max_rating(r, ix);
        int j = 0;
        while (j < ix.size()) {
            int delta = ix.size() - j;
            int j0 = j;
            if (delta >= 7) {
                j += 5;
            } else if (delta >= 6) {
                j += 4;
            } else {
                j += delta;
            }
            moves++;
            move_log.push_back(j - j0);
            for (; j0 < j; j0++) {
                move_log.push_back(ix[j0]);
                r[ix[j0]]--;
            }
        }
    }
    cout << r[0] << "\n" << moves << "\n";
    print_move_log(n, move_log);
    return 0;
}
