#ifndef IGOR_AST_HXX
#define IGOR_AST_HXX

#include "util.hxx"

namespace igor {

struct AstBase {
public:
  virtual ~AstBase() = default;
};

template<class T>
T* node_cast(std::shared_ptr<AstBase> base) {
  return dynamic_cast<T*>(base.get());
}

struct AstConstant: public AstBase {
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

struct AstFunction: public AstBase {
  string name;

  void move_name(string &name) { this->name = std::move(name); }
};

class AST {
public:
  explicit AST(std::function<void(const string&)> error_handler);
  DELETE_COPY(AST);
  DEFAULT_MOVE(AST);
  ~AST();
  // Reset and do not delete the objects. For YYABORT.
  void reset();
  void error(const string &msg);
  // Takes ownership on success.
  bool add_function(AstFunction *f);
  void clear_intern();
  string *intern(string s);

private:
  std::function<void(const string&)> error_;
  std::unordered_map<string, AstFunction*> function_map_;
  PtrUnorderedSet<string> name_intern_;
};

}  // namespace

#endif
