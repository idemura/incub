#include "base.hxx"

#include <cstdlib>
#include <cstring>
#include <cstdio>
#include <unistd.h>

namespace igor {

void check_failed(const char *file, int line, const char *text) {
  cerr<<TermColor::Red<<"CHECK FAILED: "<<file<<":"<<line<<": "<<text
      <<TermColor::Reset<<endl;
  std::exit(-1);
}

void tests_passed(const char *file) {
  cout<<TermColor::Grn<<"TESTS PASSED: "<<file
      <<TermColor::Reset<<endl;
}

namespace {
bool hyphen(char *s) {
  return s[0] == '-' && s[1] == 0;
}
}  // namespace

namespace {
class Flags {
 public:
  Flags() = default;
  // Delete copy and move constructor becase of way we destroy objects.
  DELETE_COPY(Flags);
  DELETE_MOVE(Flags);
  ~Flags() {
    // Let's release all memory of strings to be good on detecting leaks.
    for (auto &p : flags_) {
      if (p.second.type == Type::kString) {
        *reinterpret_cast<string*>(p.second.p) = string();
      }
    }
  }

  bool parse(int *argc, char **argv) {
    auto k = 1, i = 1;
    while (i < *argc) {
      auto a = argv[i];
      if (hyphen(a)) {
        i++;
        if (i < *argc) {
          argv[k++] = argv[i++];
        }
      } else if (*a == '-') {
        if (!parse_flag(*argc, argv, i, &i)) {
          return false;
        }
      } else {
        argv[k++] = argv[i++];
      }
    }
    *argc = k;
    for (; k < *argc; k++) {
      argv[k] = nullptr;
    }
    return true;
  }

  void register_flag(const char* name, i32 *f) {
    insert(name, f, Type::kInt32);
  }
  void register_flag(const char* name, i64 *f) {
    insert(name, f, Type::kInt64);
  }
  void register_flag(const char* name, bool *f) {
    insert(name, f, Type::kBool);
  }
  void register_flag(const char* name, string *f) {
    insert(name, f, Type::kString);
  }
  void register_flag(const char* name, double *f) {
    insert(name, f, Type::kDouble);
  }

 private:
  enum class Type { kNull,
    kBool,
    kInt32,
    kInt64,
    kString,
    kDouble,
  };

  struct TypedPtr {
    Type type = Type::kNull;
    void *p = nullptr;

    TypedPtr() = default;
    TypedPtr(Type type, void *p): type(type), p(p) {}
  };

  void insert(const char *name, void *p, Type type) {
    auto res = flags_.emplace(string(name), TypedPtr(type, p));
    if (not res.second) {
      cerr<<"Duplicate flag "<<name<<endl;
      std::exit(-1);
    }
  }

  bool parse_flag(int argc, char **argv, int i, int *i_out) {
    auto a = argv[i];
    if (*a == '-') a++;
    if (*a == '-') a++;
    if (*a == 0) return false;
    auto it = flags_.find(a);
    if (flags_.end() == it) {
      int a_len = std::strlen(a);
      if (a_len > 0 && (a[a_len - 1] == '-' || a[a_len - 1] == '+')) {
        auto a_short = string(a, a_len - 1);
        it = flags_.find(a_short);
        if (flags_.end() != it) {
          auto typed_ptr = it->second;
          if (typed_ptr.type == Type::kBool) {
            *(bool*)typed_ptr.p = a[a_len - 1] == '+';
            *i_out = i + 1;
            return true;
          }
          cerr<<"Non-bool flag "<<a_short<<" assigned to bool value"<<endl;
          return false;
        }
      }
      cerr<<"Unknown flag: "<<a<<endl;
      return false;
    }
    auto typed_ptr = it->second;
    if (typed_ptr.type == Type::kBool) {
      *(bool*)typed_ptr.p = true;
      *i_out = i + 1;
      return true;
    }
    i++;
    // Hyphen means next @argv value treated "as-is" without check for -/--.
    auto allow_hyphen = false;
    if (i < argc && hyphen(argv[i])) {
      i++;
      allow_hyphen = true;
    }
    if (i >= argc || (!allow_hyphen && argv[i][0] == '-')) {
      cerr<<"Missing flag value: "<<a<<endl;
      return false;
    }
    if (it->second.type == Type::kString) {
      *(string*)typed_ptr.p = argv[i];
      *i_out = i + 1;
      return true;
    }
    auto n = 0, sr = 0;
    switch (typed_ptr.type) {
      case Type::kInt32:
        sr = std::sscanf(argv[i], kI32f "%n", (i32*)typed_ptr.p, &n);
        break;
      case Type::kInt64:
        sr = std::sscanf(argv[i], kI64f "%n", (i64*)typed_ptr.p, &n);
        break;
      case Type::kDouble:
        sr = std::sscanf(argv[i], "%lf%n", (double*)typed_ptr.p, &n);
        break;
      default: break;
    }
    if (sr != 1 || n != std::strlen(argv[i])) {
      cerr<<"Invalid flag format: "<<a<<endl;
      return false;
    }
    *i_out = i + 1;
    return true;
  }

  std::unordered_map<string, TypedPtr> flags_;
};

static Flags *flags_instance;

static Flags *flags() {
  // std::once didn't work, throws std::system_error.
  static bool first = true;
  if (first) {
    flags_instance = new Flags;
    first = false;
  }
  return flags_instance;
}
}  // namespace

bool flags_parse(int *argc, char **argv) {
  return flags()->parse(argc, argv);
}

void flags_reset() {
  delete flags_instance;
  flags_instance = nullptr;
}

void flags_register(const char *name, i32 *f) {
  flags()->register_flag(name, f);
}

void flags_register(const char* name, i64 *f) {
  flags()->register_flag(name, f);
}

void flags_register(const char* name, bool *f) {
  flags()->register_flag(name, f);
}

void flags_register(const char* name, string *f) {
  flags()->register_flag(name, f);
}

void flags_register(const char* name, double *f) {
  flags()->register_flag(name, f);
}

TempFile::TempFile(const string &str) {
  char temp_name[] = "/tmp/igor.XXXXXX";
  const auto fd = mkostemp(temp_name, 0600);
  if (fd < 0) {
    return;
  }
  write(fd, str.data(), str.size());
  close(fd);
  name_ = temp_name;
}

TempFile::~TempFile() {
  if (!name_.empty()) {
    unlink(name_.c_str());
  }
}

const TermColor TermColor::Red("\x1B[31m");
const TermColor TermColor::Grn("\x1B[32m");
const TermColor TermColor::Yel("\x1B[33m");
const TermColor TermColor::Blu("\x1B[34m");
const TermColor TermColor::Mag("\x1B[35m");
const TermColor TermColor::Cyn("\x1B[36m");
const TermColor TermColor::Wht("\x1B[37m");
const TermColor TermColor::Reset("\x1B[0m");

STREAM_OUT(TermColor tc) {
  if (&os == &std::cout && isatty(1)) {
    return os<<tc.code();
  }
  if (&os == &std::cerr && isatty(2)) {
    return os<<tc.code();
  }
  return os;
}

}  // namespace
