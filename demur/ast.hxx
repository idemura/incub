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
  std::unique_ptr<AstType> clone() const;
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

struct Error {
  Error(int line, int column, string msg)
    : line(line), column(column), msg(std::move(msg)) {}
  DEFAULT_COPY(Error);

  const int line;
  const int column;
  const string msg;
};

class ErrorSink {
public:
  class Formatter {
  public:
    Formatter(ErrorSink *sink, int line, int column);
    DEFAULT_COPY(Formatter);
    DEFAULT_MOVE(Formatter);
    ~Formatter() {
      sink_->push(std::make_unique<Error>(line_, column_, ss_->str()));
    }
    std::ostream &operator*() const { return *ss_; }

  private:
    ErrorSink *sink_ = nullptr;
    const int line_;
    const int column_;
    std::unique_ptr<Error> e_;
    std::unique_ptr<std::stringstream> ss_;
  };

  explicit ErrorSink(string file): file_(std::move(file)) {}
  Formatter format_err(int line, int column) {
    return Formatter(this, line, column);
  }
  int err_count() const { return errors_.size(); }
  void print_to_stderr(bool locations) const;
  const string &get_err_msg(int i) const { return errors_[i]->msg; }

private:
  void push(std::unique_ptr<Error> e) {
    errors_.push_back(std::move(e));
  }

  std::vector<std::unique_ptr<Error>> errors_;
  const string file_;
};

// Ast is a grammar parse tree. It is a subject to semantic analysis.
class AST {
public:
  explicit AST(ErrorSink *es): es_(es) {}
  DELETE_COPY(AST);
  DEFAULT_MOVE(AST);
  // Reset and do not delete the objects. For YYABORT.
  void reset();
  void error(int line, int column, const string &msg);
  void add_function(std::unique_ptr<AstFunction> f);
  bool analyze_semantic();

private:
  void analyze_function(AstFunction *f);

  ErrorSink *const es_ = nullptr;
  std::vector<std::unique_ptr<AstFunction>> functions_;
};

}  // namespace

#endif
