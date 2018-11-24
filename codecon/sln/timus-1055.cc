#include <algorithm>
#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>

using namespace std;

using i64 = long long int;

unordered_map<int, int> fact_decomp(int n) {
    unordered_map<int, int> fd;
    vector<char> a(n + 1);
    for (int i = 2; i < a.size(); i++) {
        if (a[i]) continue;
        for (auto s = i;;) {
            for (int j = s; j < a.size(); j += s) {
                fd[i]++;
                a[j] = 1;
            }
            // This if avoids overflow of int.
            if (n / i >= s) {
                s *= i;
            } else {
                break;
            }
        }
    }
    return fd;
}

int main() {
    int n = 0, m = 0;
    cin >> n >> m;
    auto n_fd = fact_decomp(n);
    for (auto p : fact_decomp(m)) {
        n_fd[p.first] -= p.second;
    }
    for (auto p : fact_decomp(n - m)) {
        n_fd[p.first] -= p.second;
    }
    auto div_count = 0;
    for (auto p : n_fd) {
        if (p.second) div_count++;
    }
    cout << div_count << endl;
    return 0;
}
