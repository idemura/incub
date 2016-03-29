#ifndef IGOR_AST_HXX
#define IGOR_AST_HXX

#include "lexer.hxx"

namespace igor {

class AstNode;
class Visitor;

class Object {
public:
};

// use dynamic_cast if needed.
// enum class Node {
//   Function,
// };

class AstNode {
public:
  AstNode() = default;
  virtual ~AstNode() { delete_children(); }
  virtual bool accept(Visitor *visitor) = 0;
  virtual bool generate_code(Object *object) = 0;

  void add(std::unique_ptr<AstNode> node) {}
  AstNode *parent() const { return parent_; }
  AstNode *first_child() const { return first_child_; }
  AstNode *next() const { return next_; }
  AstNode *last() const { return last_; }

private:
  void delete_children();

  AstNode *parent_ = nullptr;
  AstNode *first_child_ = nullptr;
  AstNode *last_ = nullptr;
  AstNode *next_ = nullptr;
  DELETE_COPY(AstNode);
};

class AstModule: public AstNode {
public:
  AstModule() = default;
  bool accept(Visitor *visitor) override { return true; }
  bool generate_code(Object *object) override { return true; }

  string name;

private:
};

// Generic AstType: type name and possibly list of type parameters (if type
// is a template).
struct AstType {
  AstType() = default;
  explicit AstType(string name): name(std::move(name)) {}
  AstType(const AstType &other): name(other.name) {
    for (const auto &p : other.params) {
      params.push_back(p->clone());
    }
  }
  std::unique_ptr<AstType> clone() const {
    return std::make_unique<AstType>(*this);
  }

  string name;
  std::vector<std::unique_ptr<AstType>> params;
};

struct AstVarType {
  string name;
  std::unique_ptr<AstType> type;
  // how we represent type? as some tree as well?
  //string type_name;
  //return var name, return var_type;
};

struct AstVarList {
  std::vector<AstVarType> vars;
};

struct AstFnProto {
  AstVarList args, result;
};

// Function type! Int[] - array.
// fn: Int(Int, Int) function Int, Int => Int
// how we do that if function that returns several results???
// (Int, Int) => (Int, Int)
// Int => Int
// but then how we write function itself?
// function sum(x, y: Int) => Int, Int {}
// function sum(x, y: Int) => a: Int, b: Int {}
// function sum(x, y: Int) =>
//     a: Int,
//     b: Int {
//   a = x + y;
//   b = x - y;
// }
// void function:
// function check(x, y: Int) {}

class AstFunction: public AstNode {
public:
  AstFunction() = default;
  bool accept(Visitor *visitor) override { return true; }
  bool generate_code(Object *object) override { return true; }

  string name;
  AstFnProto proto;

  // name
  // args
  // returns
  // children: expressions (inner types?)

private:
};

class Visitor {
public:
  virtual ~Visitor() = default;
  virtual bool visit(AstFunction*) = 0;

protected:
  bool visit_children(AstNode *node);
};

std::unique_ptr<AstNode> build_ast(TokenStream *tokens, ErrStr &err);

}

#endif
