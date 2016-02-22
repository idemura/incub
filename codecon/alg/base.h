#ifndef BASE_H
#define BASE_H

#include <algorithm>
#include <functional>
#include <list>
#include <map>
#include <set>
#include <chrono>
#include <unordered_map>
#include <memory>
#include <vector>
#include <string>
#include <random>
#include <limits>
#include <sstream>
#include <fstream>
#include <utility>
#include <iostream>
#include <cstdlib>
#include <cstring>
#include <cmath>  // Overloads for abs.

#define ARRAY_SIZEOF(A) (sizeof(A) / sizeof(A[0]))

#define DEFINE_COPY(C, OP) \
    C(const C&) = OP; \
    C& operator=(const C&) = OP;
#define DEFAULT_COPY(C) DEFINE_COPY(C, default)
#define NON_COPYABLE(C) DEFINE_COPY(C, delete)

#define CHECK(E) \
    do { \
      if (!(E)) { \
        cout << "CHECK failed at " << __FILE__ << "@" << __LINE__ << endl; \
        exit(EXIT_FAILURE); \
      } \
    } while (false)

using namespace std;

using i64 = long long int;
using u64 = unsigned i64;
using i32 = int;
using u32 = unsigned i32;

constexpr char kEol[] = "\n";
constexpr int INF = 0x7fffffff;
constexpr int MOD = 1000000007;

template<class T>
unique_ptr<T> wrap_unique(T *p) {
  return unique_ptr<T>(p);
}

void write_string(const string &s, const string &file_name) {
  ofstream f(file_name);
  if (f.is_open()) {
    f << s;
    f.close();
  } else {
    cerr << "Failed to write file: " << file_name << endl;
    exit(1);
  }
}

#endif // BASE_H
