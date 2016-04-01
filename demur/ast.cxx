#include "ast.hxx"

#include "flags.hxx"

namespace igor {
namespace {

struct NoMatch {};
struct NoParse {};

template<class T>
struct ParseT {
  // Non-explicit for reason.
  ParseT() = default;
  DEFAULT_MOVE(ParseT);
  ParseT(NoMatch): parse(true) {}
  ParseT(NoParse): match(true) {}
  ParseT(TokenCursor c, std::unique_ptr<T> node)
      : match(true), parse(true), c(c), node(std::move(node)) {}

  bool match = false;
  bool parse = false;
  TokenCursor c;
  std::unique_ptr<T> node;
};

const auto kNoMatch = NoMatch();
const auto kNoParse = NoParse();

bool parse_var_list(TokenCursor *c, AstVarList *vars, TokenErr &err) {
  if (c->type_at() != TokType::Name) {
    err.error(*c)<<"var list: expected name\n";
  }
  vars->vars
}

// Parses x[, y]*: Type.
bool parse_arg_list(TokenCursor *c, AstVarList *vars, TokenErr &err) {
  std::vector<string> names;
  for (;;) {
    if (c->type_at() != TokType::Name) {
      err.error(*c)<<"arguments: variable name expected, got "<<*c->at()<<"\n";
      return false;
    }
    if (!check_name_at(c, err)) {
      return false;
    }
    names.push_back(get_payload<string>(c));
    if (c->type_at() == TokType::Comma) {
      c->next();
    } else if (c->type_at() == TokType::Colon) {
      c->next();
      break;
    } else {
      err.error(*c)<<"arguments: : or , expected, got "<<*c->at()<<"\n";
      return false;
    }
  }
  auto type = parse_type(c, err);
  if (nullptr == type) {
    return false;
  }
  for (auto s : names) {
    vars->vars
  }
  return true;
}

AstType *parse_type(TokenCursor *c, TokenErr &err) {
  if (c->type_at() != TokType::Name) {
    err.error(*c)<<"type def: expected type name\n";
    return nullptr;
  }
  if (!check_type_name_at(*c, err)) {
    return nullptr;
  }
  auto name = get_payload<string>(*c);
  return new AstType(name);
}

bool parse_proto(TokenCursor *c, AstFnProto *proto, TokenErr &err) {
  if (c->type_at() != TokType::LParen) {
    err.error(*c)<<"expected ( as function argument list beginning\n";
    return false;
  }
  c->next();
  for (;;) {
    if (c->type_at() == TokType::RParen) {
      c->next();
      break;
    }
    if (c->type_at() != TokType::Name) {
      err.error(*c)<<"expected first argument name\n";
      return false;
    }
    std::vector<string> names;
    for (;;) {
      names.push_back(get_payload<string>(*c));
      c->next();
      if (c->type_at() != TokType::Comma) {
        break;
      }
      c->next();
      if (c->type_at() != TokType::Name) {
        err.error(*c)<<"argument argument name after ,\n";
        return false;
      }
    }
    if (c->type_at() != TokType::Colon) {
      err.error(*c)<<"expected : after argument list\n"
      return false;
    }
    auto type = parse_type(c, err);
    if (type == nullptr) {
      return false;
    }
  }
  return true;
}

ParseT<AstNode> parse_function(TokenCursor c, TokenErr &err) {
  if (c.type_at() != TokType::Function) {
    return kNoMatch;
  }
  c.next();
  if (c.type_at() != TokType::Name) {
    err.error(c)<<"function name expected, got "<<c<<"\n";
    return kNoParse;
  }
  if (!check_name_at(c, err)) {
    return kNoParse;
  }
  auto node = std::make_unique<AstFunction>();
  node->name = get_payload<string>(*c.at());
  c.next();
  if (!parse_proto(&c, &node->proto, err)) {
    return kNoParse;
  }
  if (c.type_at() != TokType::LCurly) {
    err.error(c)<<"in function definition { expected, got "<<c<<"\n";
    return kNoParse;
  }
  c.next();
  if (c.type_at() != TokType::RCurly) {
    err.error(c)<<"in function definition } expected, got "<<c<<"\n";
    return kNoParse;
  }
  c.next();
  if (flags().log_parse) cout<<"parse: function "<<name<<endl;
  return {c, std::move(node)};
}

ParseT<AstNode> parse_top(TokenCursor c, TokenErr &err) {
  if (c.type_at() == TokType::EndFile) {
    if (flags().log_parse) cout<<"parse: end of file"<<endl;
    c.next();
    return {c, nullptr};
  }
  ParseT<AstNode> r;
  r = parse_function(c, err);
  if (r.match) {
    return r;
  }
  return kNoMatch;
}

ParseT<AstNode> parse_module(TokenCursor c, TokenErr &err) {
  if (c.type_at() != TokType::Module) {
    return kNoMatch;
  }
  c.next();
  if (c.type_at() != TokType::Name) {
    err.error(c)<<"module name expected, got "<<c<<"\n";
    return kNoParse;
  }
  if (!check_name_at(c, err)) {
    return kNoParse;
  }
  auto name = get_payload<string>(*c.at());
  c.next();
  if (c.type_at() != TokType::SemiColon) {
    err.error(c)<<"module definition should with ;, got "<<c<<"\n";
    return kNoParse;
  }
  auto node = std::make_unique<AstModule>();
  node->name = std::move(name);
  return {c, std::move(node)};
}
}  // namespace

void AstNode::delete_children() {
  for (auto n = first_child_; n != nullptr; n = n->next_) {
    n->delete_children();
    delete n;
  }
  first_child_ = last_ = nullptr;
}

std::unique_ptr<AstNode> build_ast(TokenStream *tokens, ErrStr &err) {
  TokenErr token_err(tokens->file_name(), err);
  std::unique_ptr<AstNode> module;
  auto c = tokens->cursor();
  auto r_module = parse_module(c, token_err);
  if (!r_module.parse) {
    return nullptr;
  }
  if (!r_module.match) {
    module = std::make_unique<AstModule>();
    // With empty name by default.
  } else {
    module = std::move(r_module.node);
  }
  for (;;) {
    auto r = parse_top(c, token_err);
    if (!r.match) {
      token_err.error(c)<<"can't match top level grammar\n";
      return nullptr;
    }
    if (!r.parse) {
      break;
    }
    c = r.c;
    module->add(std::move(r.node));
    if (c.done()) break;
  }
  return std::move(module);
}

void destroy_type(AstType *type) {
  if (type != nullptr) type->destroy();
}

}
