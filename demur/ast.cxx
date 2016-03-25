#include "ast.hxx"

#include "flags.hxx"

namespace igor {
namespace {

struct NoMatch {};
struct NoParse {};

struct ParseT {
  // Non-explicit for reason.
  ParseT() = default;
  ParseT(NoMatch) {}
  ParseT(NoParse): match(true) {}
  ParseT(TokenCursor c): match(true), parse(true), c(c) {}

  bool match = false;
  bool parse = false;
  TokenCursor c;
};

const auto kNoMatch = ParseT(NoMatch());
const auto kNoParse = ParseT(NoParse());

ParseT build_function(TokenCursor c, TokenErr &err) {
  if (c.at()->type != TokType::Function) {
    return kNoMatch;
  }
  c.next();
  if (c.at()->type != TokType::Name) {
    err.error(c)<<"function name expected, got "<<c<<"\n";
    return kNoParse;
  }
  const auto name = get_payload<string>(*c.at());
  string name_err;
  if (flags().check_names && !check_name(name, &name_err)) {
    err.error(c)<<name_err<<"\n";
    return kNoParse;
  }
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
  return c;
}

ParseT build_top(TokenCursor c, TokenErr &err) {
  if (c.at()->type == TokType::EndFile) {
    if (flags().log_parse) cout<<"parse: end of file"<<endl;
    c.next();
    return c;
  }
  ParseT r;
  r = build_function(c, err);
  if (r.match) {
    return r;
  }
  return kNoMatch;
}
}

void build_ast(TokenStream *tokens, ErrStr &err) {
  TokenErr token_err(tokens->file_name(), err);
  auto c = tokens->cursor();
  for (;;) {
    auto r = build_top(c, token_err);
    if (!r.match) {
      token_err.error(c)<<"can't match top\n";
      return;
    }
    if (!r.parse) {
      break;
    }
    c = r.c;
    if (c.done()) break;
  }
}

}
