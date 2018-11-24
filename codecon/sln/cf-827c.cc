#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <iostream>
#include <map>
#include <memory>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

using namespace std;

using i32 = int32_t;
using i64 = int64_t;
using pii = std::pair<int, int>;

struct binary_index_tree {
    vector<int> bit;

    void resize(int n) {
        bit.resize(n);
    }

    void add(int i, int amount) {
        i++; // Make 1-based
        while (i <= bit.size()) {
            bit[i - 1] += amount;
            i += step(i);
        }
    }

    // @i excluded
    int count(int i) const {
        if (bit.empty()) {
            return 0;
        }
        int r{0};
        // 1-based, but because excluding i we have (i + 1) - 1.
        while (i > 0) {
            r += bit[i - 1];
            i -= step(i);
        }
        return r;
    }

    // @i included, @j excluded
    int count(int i, int j) const {
        return count(j) - count(i);
    }

    // @i is 1-based index
    static int step(int i) {
        return i & -i;
    }
};

string dna;
binary_index_tree pos[4][10][10];

int char_to_code(int ch) {
    switch (ch) {
    case 'A':
        return 0;
    case 'C':
        return 1;
    case 'G':
        return 2;
    case 'T':
        return 3;
    }
    return -1;
}

void replace(int j, int c) {
    for (int i = 1; i <= 10; i++) {
        pos[dna[j]][i - 1][j % i].add(j, -1);
    }
    dna[j] = c;
    for (int i = 1; i <= 10; i++) {
        pos[dna[j]][i - 1][j % i].add(j, 1);
    }
}

void query(int a, int b, string const &buf) {
    int r{0};
    for (int i = 0; i < buf.length(); i++) {
        int c = char_to_code(buf[i]);
        r += pos[c][buf.length() - 1][(i + a) % buf.length()].count(a, b + 1);
    }
    cout << r << "\n";
}

int main() {
    cin >> dna;
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 10; j++) {
            for (int k = 0; k <= j; k++) {
                pos[i][j][k].resize(dna.length());
            }
        }
    }
    for (int j = 0; j < dna.length(); j++) {
        dna[j] = char_to_code(dna[j]);
        for (int i = 1; i <= 10; i++) {
            pos[dna[j]][i - 1][j % i].add(j, 1);
        }
    }
    int q = 0;
    cin >> q;
    while (q-- > 0) {
        int t = 0, a = 0, b = 0;
        string buf;
        cin >> t;
        if (t == 1) {
            cin >> a >> buf;
            replace(a - 1, char_to_code(buf[0]));
        } else {
            cin >> a >> b >> buf;
            query(a - 1, b - 1, buf);
        }
    }
    return 0;
}
