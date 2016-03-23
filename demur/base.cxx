#include "base.hxx"

#include <cstdlib>
#include <cstdio>  // sscanf

namespace igor {

void check_fail_report(const char *file, int line, const char *text) {
  cerr<<file<<":"<<line<<": "<<text<<endl;
  std::exit(-1);
}

u64 Substr::hash() const {
  CHECK(false);
  return -1;
}

namespace {
bool hyphen(char *s) {
  return s[0] == '-' && s[1] == 0;
}
}  // namespace

void FlagSet::insert(const string &name, Type type, void *p) {
  if (flags_.find(name) != flags_.end()) {
    cerr<<"Duplicate flag "<<name<<endl;
    exit(-1);
  }
  flags_[name] = {type, p};
}

bool FlagSet::parse_flag(int i, int argc, char **argv, int *i_out) {
  auto a = argv[i];
  if (*a == '-') a++;
  if (*a == '-') a++;
  if (*a == 0) return false;
  auto it = flags_.find(a);
  if (it == flags_.end()) {
    int l = strlen(a);
    if (a[l - 1] == '-' || a[l - 1] == '+') {
      it = flags_.find(string(a, l - 1));
      if (it != flags_.end() && it->second.type == Type::kBool) {
        *(bool*)it->second.p = a[l - 1] == '+';
        *i_out = i + 1;
        return true;
      }
    }
    cerr<<"Invalid flag: "<<a<<endl;
    return false;
  }
  auto tp = it->second;
  if (tp.type == Type::kBool) {
    *(bool*)tp.p = true;
    *i_out = i + 1;
    return true;
  }
  i++;
  auto allow_hyphen = false;
  if (i < argc && hyphen(argv[i])) {
    i++;
    allow_hyphen = true;
  }
  if (i >= argc || (!allow_hyphen && argv[i][0] == '-')) {
    cerr<<"Missing value for: "<<a<<endl;
    return false;
  }
  if (it->second.type == Type::kString) {
    ((string*)tp.p)->assign(argv[i]);
    *i_out = i + 1;
    return true;
  }
  int n = 0, sr = 0;
  switch (tp.type) {
    case Type::kInt32:
      sr = std::sscanf(argv[i], kI32f "%n", (i32*)tp.p, &n);
      break;
    case Type::kInt64:
      sr = std::sscanf(argv[i], kI64f "%n", (i64*)tp.p, &n);
      break;
    case Type::kDouble:
      sr = std::sscanf(argv[i], "%lf%n", (double*)tp.p, &n);
      break;
    default: break;
  }
  if (sr != 1 || n != strlen(argv[i])) {
    cerr<<"Invalid format: "<<a<<endl;
    return false;
  }
  *i_out = i + 1;
  return true;
}

bool FlagSet::parse(int *argc, char **argv) {
  auto k = 1, i = 1;
  while (i < *argc) {
    auto a = argv[i];
    if (hyphen(a)) {
      i++;
      if (i < *argc) {
        argv[k++] = argv[i++];
      }
    } else if (*a == '-') {
      if (!parse_flag(i, *argc, argv, &i)) {
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
}  // namespace
