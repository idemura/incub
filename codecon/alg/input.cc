#include <bits/stdc++.h>

// For 10M numbers between 0 and 1'000'000 read time:
// std::cin:  3.1s
// scanf:     1.5s
// unlocked:  0.25s

using namespace std;

using i32 = int;
using i64 = long long int;

i64 read_i64() {
    auto c = getchar_unlocked();
    while (c <= ' ') {
        c = getchar_unlocked();
    }
    auto neg = false;
    if (c == '+' || c == '-') {
        neg = c == '-';
        c = getchar_unlocked();
    }
    i64 n = 0;
    while ('0' <= c && c <= '9') {
        n = 10 * n + (c - '0');
        c = getchar_unlocked();
    }
    return neg ? -n : n;
}

i32 read_i32() {
    return read_i64();
}

double read_double() {
    auto c = getchar_unlocked();
    while (c <= ' ') {
        c = getchar_unlocked();
    }
    auto neg = false;
    if (c == '+' || c == '-') {
        neg = c == '-';
        c = getchar_unlocked();
    }
    double m = 0;
    while ('0' <= c && c <= '9') {
        m = 10.0 * m + (c - '0');
        c = getchar_unlocked();
    }
    if (c == '.') {
        c = getchar_unlocked();
        double f = 0.1;
        while ('0' <= c && c <= '9') {
            m += (c - '0') * f;
            f *= 0.1;
            c = getchar_unlocked();
        }
    }
    if (c == 'e' || c == 'E') {
        m *= pow(10.0, read_i32());
    }
    return neg ? -m : m;
}

string read_line() {
    string s;
    auto c = getchar_unlocked();
    while (c != '\n' && c != EOF) {
        s.push_back(c);
        c = getchar_unlocked();
    }
    return s;
}

int main(int argc, char **argv) {
    return 0;
}
