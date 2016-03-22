#include "lexer.hxx"

namespace igor {
namespace {
bool is_space(int c) {
  return c == ' ' || c == '\t' || c == '\n' || c == '\r';
}

bool is_valid(int c) {
  return (c >= ' ' && c < 127) || is_space(c);
}

bool is_digit(int c) {
  return '0' <= c && c <= '9';
}

class Lexer {
public:
  Lexer(TokenStream *tokens, string s, ErrStr &err)
      : tokens(tokens), s(std::move(s)), err(err) {}

  void tokenize() {
    const auto &file_name = tokens->file_name();
    int line = 1;
    int i = 0;
    while (i < s.size() && is_space(s[i])) {
      if (!is_valid(s[i])) {
        err.error(file_name, line)<<" invalid character at "<<(i + 1)<<"\n";
        return;
      }
      if (s[i] == '\n') line += 1;
      i++;
    }
    if (i == s.size()) {
      tokens->add(std::make_unique<Token>(line, i + 1, TokType::EndFile));
      return;
    }
  }

private:
  TokenStream* const tokens = nullptr;
  string s;
  int line = 0;
  int i = 0;
  ErrStr &err;
};
}

void TokenStream::add(std::unique_ptr<Token> t) {
  tokens_.push_back(std::move(t));
}

std::unique_ptr<TokenStream> tokenize(
    const std::string &file_name,
    std::string s,
    ErrStr &err) {
  auto tokens = std::make_unique<TokenStream>(file_name);
  Lexer lexer(tokens.get(), std::move(s), err);
  lexer.tokenize();
  return std::move(tokens);
}

}
