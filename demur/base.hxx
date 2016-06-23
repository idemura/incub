#ifndef IGOR_BASE_HXX
#define IGOR_BASE_HXX

#include <algorithm>
#include <atomic>
#include <chrono>
#include <functional>
#include <fstream>
#include <future>
#include <iostream>
#include <map>
#include <memory>
#include <mutex>
#include <sstream>
#include <string>
#include <thread>
#include <unordered_map>
#include <utility>
#include <vector>

#define ARRAY_SIZEOF(A) (sizeof(A) / sizeof(A[0]))

#define DEFINE_COPY(C, OP) \
    C(const C&) = OP;\
    C& operator=(const C&) = OP;
#define DEFINE_MOVE(C, OP) \
    C(C&&) = OP;\
    C& operator=(C&&) = OP;
#define DELETE_COPY(C) DEFINE_COPY(C, delete)
#define DEFAULT_COPY(C) DEFINE_COPY(C, default)
#define DELETE_MOVE(C) DEFINE_MOVE(C, delete)
#define DEFAULT_MOVE(C) DEFINE_MOVE(C, default)
#define NON_COPYABLE(C) DELETE_COPY(C)

#define CHECK_MSG(E, T) \
    do {\
      if (!(E)) {\
        igor::check_failed(__FILE__, __LINE__, T);\
      }\
    } while (false)

#define CHECK(E) CHECK_MSG(E, "check failed: " #E)
#define FATAL(T) CHECK_MSG(false, T)

#define RETURN_TESTS_PASSED() \
    do {\
      igor::tests_passed(__FILE__);\
      return 0;\
    } while (false)

#define kI32f "%d"
#define kI64f "%lld"

#define STREAM_OUT(ARG) inline std::ostream &operator<<(std::ostream &os, ARG)

// DEFINE_FLAG should be in the global namespace.
#define DEFINE_FLAG(NAME, VALUE, TYPE) \
    TYPE flag_##NAME = VALUE;\
    namespace igor {\
    namespace flag_auto_reg {\
      struct AutoReg_##NAME {\
        AutoReg_##NAME() { flags_register(#NAME, &::flag_##NAME); }\
      } NAME;\
    }}

#define FLAG_bool(NAME, VALUE)    DEFINE_FLAG(NAME, VALUE, bool)
#define FLAG_i32(NAME, VALUE)     DEFINE_FLAG(NAME, VALUE, igor::i32)
#define FLAG_i64(NAME, VALUE)     DEFINE_FLAG(NAME, VALUE, igor::i64)
#define FLAG_string(NAME, VALUE)  DEFINE_FLAG(NAME, VALUE, string)
#define FLAG_double(NAME, VALUE)  DEFINE_FLAG(NAME, VALUE, double)

using std::cerr;
using std::cout;
using std::endl;
using std::string;

namespace igor {

using i32 = int;
using i64 = long long int;
using u32 = unsigned i32;
using u64 = unsigned i64;

bool flags_parse(int *argc, char **argv);
void flags_reset();
void flags_register(const char *name, i32 *f);
void flags_register(const char *name, i64 *f);
void flags_register(const char *name, bool *f);
void flags_register(const char *name, string *f);
void flags_register(const char *name, double *f);

// Generally, shouldn't be used, but sometimes convenient.
template<class T>
std::unique_ptr<T> wrap_unique(T *p) {
  return std::unique_ptr<T>(p);
}

class ErrStr {
public:
  ErrStr() {}
  DEFAULT_COPY(ErrStr);
  DEFAULT_MOVE(ErrStr);

  string str() { return ss_.str(); }
  std::stringstream &error(const string &file, int line, int col) {
    err_count_++;
    ss_<<file<<":"<<line;
    if (col > 0) ss_<<":"<<col;
    ss_<<" error: ";
    return ss_;
  }
  bool ok() const { return err_count_ == 0; }
  void clear_error() {
    err_count_ = 0;
    // Clear @ss_. Otherwise, when we add a new error and print it, a weird
    // message will appear.
    ss_.str(std::string());
  }

private:
  std::stringstream ss_;
  int err_count_ = 0;
};

void check_failed(const char *file, int line, const char *text);
void tests_passed(const char *file);

class TempFile {
public:
  explicit TempFile(const string &str);
  DEFAULT_COPY(TempFile);
  DEFAULT_MOVE(TempFile);
  ~TempFile();
  string get_name() const { return name_; }

private:
  string name_;
};

}  // namespace

#endif
