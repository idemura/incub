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

// Generic AstType: type name and possibly list of parameters (if type is a
// generic type), which are named "instance types"
struct AstType {
  AstType() = default;
  explicit AstType(string name): name(std::move(name)) {}
  AstType(const AstType &other)
      : name(other.name),
        inst_types(other.inst_types) {
    for (auto &p : inst_types) p = p->clone();
  }
  AstType* clone() const {
    return new AstType(*this);
  }
  void destroy() {
    for (auto p : inst_types) p->destroy();
    delete this;
  }

  string name;
  std::vector<AstType*> inst_types;
};

void destroy_type(AstType *type);

struct AstVarType {
  explicit AstVarType(string name, AstType *type): name(name), type(type) {}
  ~AstVarType() { destroy_type(type); }
  DELETE_COPY(AstVarType);
  DEFAULT_MOVE(AstVarType);

  string name;
  AstType *type = nullptr;
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
AstType *parse_type(TokenCursor *c, TokenErr &err);

}

#endif
