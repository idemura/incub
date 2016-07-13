#ifndef IGOR_AST_HXX
#define IGOR_AST_HXX

#include "util.hxx"

namespace igor {

struct AstBase {
  virtual ~AstBase() = default;
  virtual void to_string(std::ostream &os) const { ss<<"(AstBase)"; }
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

  void to_string(std::ostream &os) const override;
  std::unique_ptr<AstType> clone() const;
};

struct AstArg: public AstBase {
  string name;
  std::unique_ptr<AstType> type;

  void to_string(std::ostream &os) const override {
    ss<<"(Arg "<<name<<" ";
    type->to_string(os);
    ss<<")";
  }
};

struct AstArgList: public AstBase {
  // Args are in reverse order.
  std::vector<std::unique_ptr<AstArg>> args;

  void to_string(std::ostream &os) const override {
    ss<<"(ArgList";
    for (auto &a : args) {
      ss<<" ";
      a.to_string(os);
    }
    ss<<")";
  }
};

// *** Expression
struct AstExpr: public AstBase {
};

struct AstExprList: public AstBase {
  std::vector<std::unique_ptr<AstExpr>> exprs;

  void to_string(std::ostream &os) const override {
    ss<<"(ArgExprList";
    for (auto &a : exprs) {
      ss<<" ";
      a.to_string(ss);
    }
    ss<<")";
  }
};

struct AstConstant: public AstExpr {
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

  AstConstant() = default;
  AstConstant(Type type, string value): type(type), value(std::move(value)) {}

  void to_string(std::ostream &os) const override {
    ss<<"(Constant "<<value<<")";
  }
};

struct AstExprNot: public AstExpr {
  AstExprNot(std::unique_ptr<AstExpr> e): e(std::move(e)) {}
  void to_string(std::ostream &os) const override {
    os<<"(Not ";
    e->

  std::unique_ptr<AstExpr> e;
};

struct AstExprAnd: public AstExpr {
  AstExprAnd(std::unique_ptr<AstExpr> l, std::unique_ptr<AstExpr> r)
      : l(std::move(l)),
        r(std::move(r)) {}
  std::unique_ptr<AstExpr> l, r;
};

struct AstExprOr: public AstExpr {
  AstExprOr(std::unique_ptr<AstExpr> l, std::unique_ptr<AstExpr> r)
      : l(std::move(l)),
        r(std::move(r)) {}
  std::unique_ptr<AstExpr> l, r;
};

struct AstExprXor: public AstExpr  {
  AstExprXor(std::unique_ptr<AstExpr> l, std::unique_ptr<AstExpr> r)
      : l(std::move(l)),
        r(std::move(r)) {}
  std::unique_ptr<AstExpr> l, r;
};

struct AstExprCall: public AstExpr {
  AstExprCall(std::unique_ptr<AstExpr> f, std::unique_ptr<AstExprList> args)
      : f(std::move(f)),
        args(std::move(args)) {}
  std::unique_ptr<AstExpr> f;
  std::unique_ptr<AstExprList> args;
};

struct AstExprVar: public AstExpr {
  AstExprVar(string name): name(std::move(name)) {}
  string name;
};

struct AstExprAt: public AstExpr {
  AstExprAt(std::unique_ptr<AstExpr> l, std::unique_ptr<AstExpr> r)
      : l(std::move(l)),
        r(std::move(r)) {}
  std::unique_ptr<AstExpr> l, r;
};

// *** Statement
struct AstStmt: public AstBase {
};

struct AstStmtReturn: public AstStmt {
  AstStmtReturn(std::unique_ptr<AstExprList> e): e(std::move(e)) {}
  std::unique_ptr<AstExprList> e;
};

struct AstStmtLet: public AstStmt {
  AstStmtLet(std::unique_ptr<AstExpr> l, std::unique_ptr<AstExpr>(r))
      : l(std::move(l)),
        r(std::move(r)) {}
  std::unique_ptr<AstExpr> l, r;
};

struct AstStmtExpr: public AstStmt {
  AstStmtExpr(std::unique_ptr<AstExpr> e): e(std::move(e)) {}
  std::unique_ptr<AstExpr> e;
};

struct AstStmtList: public AstStmt {
  std::vector<std::unique_ptr<AstStmt>> stmts;
};

struct AstFunction: public AstBase {
  string name;
  std::unique_ptr<AstArgList> arg_list;
  std::unique_ptr<AstArgList> ret_list;
  std::unique_ptr<AstStmtList> body;

  void to_string(std::ostream &ss) const override;
};

// *** AST
class ErrorMsg {
public:
  ErrorMsg(std::ostream *os, const string &file, int line, int column);
  DEFAULT_COPY(ErrorMsg);
  ~ErrorMsg() { *os_<<endl; }
  std::ostream &os() const { return *os_; }

private:
  std::ostream *const os_ = nullptr;
};

class ErrorLog {
public:
  explicit ErrorLog(string file, std::ostream &os = cerr)
      : file_(std::move(file)), os_(&os) {
  }
  DEFAULT_COPY(ErrorLog);
  ErrorMsg error(int line, int column);
  int count() const { return count_; }

private:
  const string file_;
  std::ostream *const os_ = nullptr;
  int count_ = 0;
};

// Ast is a grammar parse tree. It is a subject to semantic analysis.
class AST {
public:
  AST() = default;
  DELETE_COPY(AST);
  void add_function(std::unique_ptr<AstFunction> f);
  bool analyze(ErrorLog *elog);

private:
  void analyze_function(AstFunction *f, ErrorLog *elog);

  std::vector<std::unique_ptr<AstFunction>> functions_;
};

}  // namespace

#endif
