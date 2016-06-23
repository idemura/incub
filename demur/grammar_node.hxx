#ifndef IGOR_GRAMMAR_NODE_HXX
#define IGOR_GRAMMAR_NODE_HXX

#include "base.hxx"

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

// For both TOK_TYPE and TOK_NAME.
struct Name {
  explicit Name(string name): name(std::move(name)) {}
  string name;
};
}  // namespace

#endif
