#ifndef IGOR_AST_HXX
#define IGOR_AST_HXX

#include "util.hxx"

namespace igor {

struct AstBase {
public:
  virtual ~AstBase() = default;
  // virtual string to_string() const = 0;
};

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

// In general, type is GenericTypeName (@name) and @args.
struct AstType: public AstBase {
  string name;
  // True if type represents T1 => T2 in T => T1 => T2, false if cannot be
  // carried into a bigger function type like T1 => T2 in T => (T1 => T2).
  // Another solution?
  bool carry = true;
  // Args are in reverse order. Otherwise insertion will take O(N^2) time,
  // because grammar is tail recursive (<list_item> COMMA <list>).
  std::vector<std::unique_ptr<AstType>> args;

  string to_string() const;
};

struct AstArg: public AstBase {
  string name;
  std::unique_ptr<AstType> type;
};

struct AstArgList: public AstBase {
  // Args are in reverse order.
  std::vector<std::unique_ptr<AstArg>> args;
};

struct AstFunction: public AstBase {
  string name;
  std::unique_ptr<AstArgList> arg_list = nullptr;
  std::unique_ptr<AstArgList> ret_list = nullptr;
};

// Ast is a grammar parse tree. It is a subject to semantic analysis.
class AST {
public:
  explicit AST(std::function<void(const string&)> error_handler);
  DELETE_COPY(AST);
  DEFAULT_MOVE(AST);
  ~AST();
  // Reset and do not delete the objects. For YYABORT.
  void reset();
  void error(const string &msg);
  void add_function(std::unique_ptr<AstFunction> f);

private:
  std::function<void(const string&)> error_;
  std::vector<std::unique_ptr<AstFunction>> functions_;
};

}  // namespace

#endif
