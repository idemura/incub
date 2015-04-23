#ifndef BASE_H
#define BASE_H

#include <algorithm>
#include <functional>
#include <list>
#include <map>
#include <unordered_map>
#include <vector>
#include <string>
#include <sstream>
#include <utility>
#include <iostream>
#include <stdlib.h>
#include <math.h>

#define ARRAY_SIZEOF(A) (sizeof(A) / sizeof(A[0]))
#define NON_COPYABLE(C) \
    C(const C&) = delete; \
    C& operator=(const C&) = delete;

#define CHECK(E) \
    do { \
      if (!(E)) { \
        std::cout << "CHECK failed at " << __FILE__ << "@" << __LINE__ << std::endl; \
        exit(EXIT_FAILURE); \
      } \
    } while (false)

using std::string;
using std::cerr;
using std::cout;
using std::endl;

using i64 = long long int;
using i32 = int;

constexpr char kEol[] = "\n";
constexpr int INF = 0x7fffffff;
constexpr int MOD = 100000007;

#endif // BASE_H
