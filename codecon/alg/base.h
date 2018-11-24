#ifndef BASE_H
#define BASE_H

#include <algorithm>
#include <array>
#include <atomic>
#include <chrono>
#include <cmath> // Overloads for abs.
#include <cstddef>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <functional>
#include <iostream>
#include <limits>
#include <list>
#include <map>
#include <memory>
#include <queue>
#include <random>
#include <set>
#include <sstream>
#include <string>
#include <tuple>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#define ARRAY_SIZEOF(A) (sizeof(A) / sizeof(A[0]))

#define DEFINE_COPY(C, OP)                                                     \
    C(const C &) = OP;                                                         \
    C &operator=(const C &) = OP;
#define DEFAULT_COPY(C) DEFINE_COPY(C, default)
#define NON_COPYABLE(C) DEFINE_COPY(C, delete)

#define CHECK(E)                                                               \
    do {                                                                       \
        check_macro_fn(E, __FILE__, __LINE__);                                 \
    } while (false)

using namespace std;

using i64 = long long int;
using u64 = unsigned i64;
using i32 = int;
using u32 = unsigned i32;

constexpr char kEol[] = "\n";
constexpr int INF = 0x7fffffff;
constexpr int MOD = 1000000007;

template <class T>
unique_ptr<T> wrap_unique(T *p) {
    return unique_ptr<T>(p);
}

inline void write_string(const string &s, const string &file_name) {
    ofstream f(file_name);
    if (f.is_open()) {
        f << s;
        f.close();
    } else {
        cerr << "Failed to write file: " << file_name << endl;
        exit(1);
    }
}

inline void check_macro_fn(bool b, const char *file, int line) {
    if (!b) {
        cout << "CHECK failed at " << file << "@" << line << endl;
        exit(EXIT_FAILURE);
    }
}

#endif // BASE_H
