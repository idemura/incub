#ifndef IPED_BASE_HXX
#define IPED_BASE_HXX

#include <algorithm>
#include <cstdlib>
#include <iostream>
#include <functional>
#include <map>
#include <memory>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#define ARRAY_SIZEOF(A) (sizeof(A) / sizeof(A[0]))

#define DEFINE_COPY(C, OP) \
    C(const C&) = OP; \
    C& operator=(const C&) = OP;
#define DELETE_COPY(C) DEFINE_COPY(C, delete)
#define DEFAULT_COPY(C) DEFINE_COPY(C, default)
#define NON_COPYABLE(C) DELETE_COPY(C)

#define CHECK(E) \
    do { \
      if (!(E)) { \
        cout << "CHECK failed at " << __FILE__ << "@" << __LINE__ << endl; \
        std::exit(-1); \
      } \
    } while (false)

#define kI32f "%d"
#define kI64f "%lld"

namespace iped {

using i32 = int;
using i64 = long long int;
using u32 = unsigned i32;
using u64 = unsigned i64;

using std::cerr;
using std::cout;
using std::endl;
using std::string;

}  // namespace

#endif

