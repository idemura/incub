#include <algorithm>
#include <iostream>
#include <string>
#include <vector>

using namespace std;

bool is_balanced(const string &s) {
    int bal = 0;
    for (auto c : s) {
        switch (c) {
        case '(': {
            bal++;
            break;
        }
        case ')': {
            if (bal == 0) {
                return false;
            }
            bal--;
            break;
        }
        }
    }
    return bal == 0;
}

string dummy_longest(const string &s) {
    int max_l = 0;
    int max_i = 0;
    for (int i = 0; i < s.size(); i++) {
        for (int j = i + 1; j <= s.size(); j++) {
            if (is_balanced(s.substr(i, j - i)) && j - i > max_l) {
                max_l = j - i;
                max_i = i;
            }
        }
    }
    return s.substr(max_i, max_l);
}

string smart_longest(const string &s) {
    vector<int> imbalance, open;
    for (int i = 0; i < s.size(); i++) {
        switch (s[i]) {
        case '(': {
            open.push_back(i);
            break;
        }
        case ')': {
            if (open.empty()) {
                imbalance.push_back(i);
            } else {
                open.pop_back();
            }
            break;
        }
        }
    }
    // Position of every imbalanced ')' is less than '('.
    imbalance.insert(imbalance.end(), open.begin(), open.end());
    if (imbalance.empty()) {
        return s;
    }
    imbalance.push_back(s.size());
    int max_l = 0;
    int max_i = 0;
    int prev = 0;
    for (int i = 0; i < imbalance.size(); i++) {
        int j = imbalance[i];
        if (j - prev > max_l) {
            max_l = j - prev;
            max_i = prev;
        }
        prev = j + 1;
    }
    return s.substr(max_i, max_l);
}

void test(const string &s) {
    auto dummy = dummy_longest(s);
    auto smart = smart_longest(s);
    if (dummy != smart) {
        cerr << "FAILED: s=" << s << " dummy=" << dummy << " smart=" << smart
             << endl;
        return;
    }
}

int main() {
    test("()(()())");
    test("())(()())");
    test("()((()())");
    test("())(()())(");
    test("())(()())((())()()(");
    test("())()()(()))()())))");
    test("))");
    test("((");
    return 0;
}
