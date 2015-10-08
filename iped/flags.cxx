#include "flags.hxx"

namespace iped {
namespace {

enum class Type {
  kInt32,
  kInt64,
  kBool,
  kString,
  kDouble,
};

class Flags {
 public:
  Flags();
  bool parse(int *argc, char **argv);

  string temp_dir = ".";
  string log_file;

 private:
  struct TypedPtr {
    Type type = Type::kInt32;
    void *p = nullptr;

    TypedPtr() {}
    TypedPtr(Type type, void *p): type(type), p(p) {}
  };

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

  void insert(const string &name, Type type, void *p);

  std::unordered_map<string, TypedPtr> flags_;
  NON_COPYABLE(Flags);
};

Flags::Flags() {
  register_flag("temp_dir", &temp_dir);
  register_flag("log_file", &log_file);
}

void Flags::insert(const string &name, Type type, void *p) {
  if (flags_.find(name) != flags_.end()) {
    cerr<<"Duplicated flag "<<name<<endl;
    exit(-1);
  }
  flags_[name] = {type, p};
}

bool Flags::parse(int *argc, char **argv) {
  return true;
}

std::unique_ptr<Flags> flags;
}  // namespace

const string& flag_temp_dir() {
  return flags->temp_dir;
}
const string& flag_log_file() {
  return flags->log_file;
}

bool init_flags(int *argc, char **argv) {
  flags.reset(new Flags());
  return flags->parse(argc, argv);
}
}  // namespace
