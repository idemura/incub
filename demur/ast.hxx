#ifndef IGOR_AST_HXX
#define IGOR_AST_HXX

#include "util.hxx"

namespace igor {

struct Constant {
  enum Type {
    kNull,
    kBool,
    kI8,
    kI16,
    kI32,
    kI64,
    kF32,
    kF64,
    kChar,
    kString,
  };
  Type type = kNull;
  string value;
};

struct Function {
  string name;
};

class AST {
public:
  explicit AST(std::function<void(const string&)> error_handler);
  DELETE_COPY(AST);
  DEFAULT_MOVE(AST);
  ~AST();
  void error(const string &msg);
  bool add_function(Function *function);
  string *intern(string s);
  void clear_intern();

private:
  std::function<void(const string&)> error_;
  std::unordered_map<string, std::unique_ptr<Function>> function_map_;
  PtrUnorderedSet<string> name_intern_;
};

}  // namespace

#endif
