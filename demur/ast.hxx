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

class AstFunction: public AstNode {
public:
  AstFunction() = default;
  bool accept(Visitor *visitor) override { return true; }
  bool generate_code(Object *object) override { return true; }

  string name;

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
