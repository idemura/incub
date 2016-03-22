#ifndef IGOR_BASE_HXX
#define IGOR_BASE_HXX

#include <algorithm>
#include <cstring>
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

#define CHECK(E) do{ check_macro_fn(E, __FILE__, __LINE__); } while (false)

#define kI32f "%d"
#define kI64f "%lld"

namespace igor {

using i32 = int;
using i64 = long long int;
using u32 = unsigned i32;
using u64 = unsigned i64;

using std::cerr;
using std::cout;
using std::endl;
using std::string;

class Substr {
public:
  // For @npos, use `string::npos`.
  // Seems like constexpr can be decalred here.

  using Iter = const char*;

  Substr() : data_(nullptr), size_(0) {}
  Substr(const char *data): Substr(data, std::strlen(data)) {}
  Substr(const char *data, size_t size): data_(data), size_(0) {}
  Substr(const string &str): Substr(str.data(), str.size()) {}
  Substr(const Substr &other): Substr(other.data_, other.size_) {}
  Substr& operator=(const Substr &other) { return assign(other); }

  char operator[](size_t i) const { return data_[i]; }
  const char* data() const { return data_; }
  char* mutable_data() { return const_cast<char*>(data_); }
  size_t size() const { return size_; }
  bool empty() const { return size_ == 0; }
  char front() const { return *data_; }
  char back() const { return data_[size_ - 1]; }
  Iter begin() const { return data_; }
  Iter end() const { return data_ + size_; }

  Substr& assign(const char *data) { return assign(data, std::strlen(data)); }
  Substr& assign(const char *data, size_t size) {
    data_ = data;
    size_ = size;
    return *this;
  }
  Substr& assign(const string &str) { return assign(str.data(), str.size()); }
  Substr& assign(Substr other) { return assign(other.data_, other.size_); }

  Substr substr(size_t pos) const { return substr(pos, size_ - pos); }
  Substr substr(size_t pos, size_t count) const {
    return Substr(data_ + pos, count);
  }

  Substr range(size_t first) const { return range(first, size_); }
  Substr range(size_t first, size_t last) const {
    return Substr(data_ + first, last - first);
  }

  Substr prefix(size_t count) const {
    if (count > size_) count = size_;
    return substr(0, count);
  }
  Substr suffix(size_t count) const {
    if (count > size_) count = size_;
    return substr(size_ - count, count);
  }

  u64 hash() const;

  bool equal(Substr other) const {
    return size_ == other.size_ && std::memcmp(data_, other.data_, size_) == 0;
  }
  bool order(Substr other) const {
    auto r = std::memcmp(data_, other.data_, std::min(size_, other.size_));
    return r < 0 || (r == 0 && size_ < other.size_);
  }

private:
  const char *data_;
  size_t size_;
};

inline bool operator<(Substr l, Substr r) {
  return l.order(r);
}
inline bool operator>(Substr l, Substr r) {
  return r.order(l);
}
inline bool operator<=(Substr l, Substr r) {
  return !(r < l);
}
inline bool operator>=(Substr l, Substr r) {
  return !(l < r);
}
inline bool operator==(Substr l, Substr r) {
  return l.equal(r);
}
inline bool operator!=(Substr l, Substr r) {
  return !(l == r);
}

inline std::ostream& operator<<(std::ostream& os, Substr s) {
  return os.write(s.data(), s.size());
}

// Command line flags utilities.
class FlagSet {
 public:
  FlagSet() {}
  virtual ~FlagSet() {}
  bool parse(int *argc, char **argv);

 protected:
  void register_flag(const char* name, i32 *f) {
    insert(name, Type::kInt32, f);
  }
  void register_flag(const char* name, i64 *f) {
    insert(name, Type::kInt64, f);
  }
  void register_flag(const char* name, bool *f) {
    insert(name, Type::kBool, f);
  }
  void register_flag(const char* name, string *f) {
    insert(name, Type::kString, f);
  }
  void register_flag(const char* name, double *f) {
    insert(name, Type::kDouble, f);
  }

 private:
  enum class Type {
    kInt32,
    kInt64,
    kBool,
    kString,
    kDouble,
  };

  struct TypedPtr {
    Type type = Type::kInt32;
    void *p = nullptr;

    TypedPtr() {}
    TypedPtr(Type type, void *p): type(type), p(p) {}
  };

  void insert(const string &name, Type type, void *p);
  bool parse_flag(int i, int argc, char **argv, int *i_out);

  std::unordered_map<string, TypedPtr> flags_;
};

void check_macro_fn(bool expr, const char *file, int line);
}  // namespace

namespace std {
template<>
struct hash<igor::Substr> {
  size_t operator()(igor::Substr key) const { return key.hash(); }
};

inline string to_string(igor::Substr s) {
  return string(s.data(), s.size());
}
}  // namespace std (specialization)

#endif
