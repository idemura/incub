#ifndef IPED_FLAGS_HXX
#define IPED_FLAGS_HXX

#include "base.hxx"

namespace iped {

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

}  // namespace

#endif
