#include "ast.hxx"

#include "flags.hxx"

namespace igor {
namespace {

struct NoMatch {};
struct NoParse {};

struct ParseT {
  // Non-explicit for reason.
  ParseT() = default;
  DEFAULT_MOVE(ParseT);
  ParseT(NoMatch): parse(true) {}
  ParseT(NoParse): match(true) {}
  ParseT(TokenCursor c, std::unique_ptr<AstNode> node)
      : match(true), parse(true), c(c), node(std::move(node)) {}

  bool match = false;
  bool parse = false;
  TokenCursor c;
  std::unique_ptr<AstNode> node;
};

const auto kNoMatch = NoMatch();
const auto kNoParse = NoParse();

ParseT parse_function(TokenCursor c, TokenErr &err) {
  if (c.at()->type != TokType::Function) {
    return kNoMatch;
  }
  c.next();
  if (c.at()->type != TokType::Name) {
    err.error(c)<<"function name expected, got "<<c<<"\n";
    return kNoParse;
  }
  if (!check_name_at(c, err)) {
    return kNoParse;
  }
  auto name = get_payload<string>(*c.at());
  c.next();
  if (c.at()->type != TokType::LParen) {
    err.error(c)<<"in function definition ( expected, got "<<c<<"\n";
    return kNoParse;
  }
  c.next();
  if (c.at()->type != TokType::RParen) {
    err.error(c)<<"in function definition ) expected, got "<<c<<"\n";
    return kNoParse;
  }
  c.next();
  if (c.at()->type != TokType::LCurly) {
    err.error(c)<<"in function definition { expected, got "<<c<<"\n";
    return kNoParse;
  }
  c.next();
  if (c.at()->type != TokType::RCurly) {
    err.error(c)<<"in function definition } expected, got "<<c<<"\n";
    return kNoParse;
  }
  c.next();
  if (flags().log_parse) cout<<"parse: function "<<name<<endl;
  auto node = std::make_unique<AstFunction>();
  node->name = std::move(name);
  return {c, std::move(node)};
}

ParseT parse_top(TokenCursor c, TokenErr &err) {
  if (c.at()->type == TokType::EndFile) {
    if (flags().log_parse) cout<<"parse: end of file"<<endl;
    c.next();
    return {c, nullptr};
  }
  ParseT r;
  r = parse_function(c, err);
  if (r.match) {
    return r;
  }
  return kNoMatch;
}

ParseT parse_module(TokenCursor c, TokenErr &err) {
  if (c.at()->type != TokType::Module) {
    return kNoMatch;
  }
  c.next();
  if (c.at()->type != TokType::Name) {
    err.error(c)<<"module name expected, got "<<c<<"\n";
    return kNoParse;
  }
  if (!check_name_at(c, err)) {
    return kNoParse;
  }
  auto name = get_payload<string>(*c.at());
  c.next();
  if (c.at()->type != TokType::SemiColon) {
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

}
