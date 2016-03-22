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
      : tokens_(tokens), s_(std::move(s)), err_(err) {}

  void tokenize() {
    while (get_next()) {
      // Empty
    }
  }

private:
  std::stringstream &error() {
    return err_.error(tokens_->file_name(), line_ + 1, i_ + 1);
  }

  int at() const { return s_[i_]; }
  bool done() const { return i_ == s_.size(); }
  void next() { i_++; }

  bool get_next() {
    while (!done() && is_space(at())) {
      if (!is_valid(at())) {
        error()<<"invalid character\n";
        return false;
      }
      if (at() == '\n') line_ += 1;
      next();
    }
    if (done()) {
      tokens_->add(std::make_unique<Token>(line_, i_ + 1, TokType::EndFile));
      return false;
    }
    if (is_digit(at())) {
      return get_number();
    }
    error()<<"unknown token\n";
    return false;
  }

  bool get_number() {
    if (at() == '0') {
      i_ += 1;
      if (!done()) {
        if (at() == 'x' || at() == 'X') {
          i_++;
          return hex_integer();
        } else if (at() == 'o' || at() == 'O') {
          i_++;
          return oct_integer();
        } else if (at() == 'b' || at() == 'B') {
          i_++;
          return bin_integer();
        }
      }
      i_ -= 1;
    }
    // Decimal integer or floating point literal.
    auto first = i_;
    auto fp = -1;
    auto fp_e = -1;
    while (!done() && !is_space(at())) {
      if (is_digit(at())) {
        next();
      } else if (at() == '_') {
        next();
      } else if (at() == '.') {
        if (fp >= 0) {
          error()<<"floating point literal: duplicated point at "
                 <<(fp + 1)<<"\n";
          return false;
        }
        if (fp_e >= 0) {
          error()<<"floating point literal: point after exponent(E) at "
                 <<(fp_e + 1)<<"\n";
          return false;
        }
        fp = i_;
      } else if (at() == 'e' || at() == 'E') {
        if (fp_e >= 0) {
          error()<<"floating point literal: duplicated exponent(E) at "
                 <<(fp_e + 1)<<"\n";
          return false;
        }
      } else {

      }
    }
    return false;
  }

  bool hex_integer() {
    return false;
  }
  bool oct_integer() {
    return false;
  }
  bool bin_integer() {
    return false;
  }

  TokenStream* const tokens_ = nullptr;
  string s_;
  int line_ = 0;
  int i_ = 0;
  ErrStr &err_;
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
