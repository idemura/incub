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
#include <utility>
#include <iostream>
#include <cstdlib>
#include <cstring>
#include <cmath>  // Overloads for abs.

#define ARRAY_SIZEOF(A) (sizeof(A) / sizeof(A[0]))
#define NON_COPYABLE(C) \
    C(const C&) = delete; \
    C& operator=(const C&) = delete;
#define NEW_UNIQUE(T) unique_ptr<T>(new T)
#define CHECK(E) \
    do { \
      if (!(E)) { \
        cout << "CHECK failed at " << __FILE__ << "@" << __LINE__ << endl; \
        exit(EXIT_FAILURE); \
      } \
    } while (false)

using namespace std;

using i64 = long long int;
using i32 = int;

constexpr char kEol[] = "\n";
constexpr int INF = 0x7fffffff;
constexpr int MOD = 100000007;

template<class T>
unique_ptr<T> wrap_unique(T *p) {
  return unique_ptr<T>(p);
}

#endif // BASE_H
