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

bool is_bin_digit(int c) {
  return '0' <= c && c <= '1';
}

bool is_oct_digit(int c) {
  return '0' <= c && c <= '7';
}

bool is_hex_digit(int c) {
  return ('0' <= c && c <= '9') ||
         ('a' <= c && c <= 'f') ||
         ('A' <= c && c <= 'F');
}

bool is_alpha(int c) {
  return ('a' <= c && c <= 'z') ||
         ('A' <= c && c <= 'Z');
}

bool is_alnum(int c) {
  return is_alpha(c) || is_digit(c) || c == '_';
}

bool is_upper(int c) {
  return 'A' <= c && c <= 'Z';
}

bool is_lower(int c) {
  return 'a' <= c && c <= 'z';
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
    return err_.error(tokens_->file_name(), line_, i_ + 1);
  }

  int at() const { return s_[i_]; }
  bool done() const { return i_ == s_.size(); }
  void next() { i_++; }
  void back() { i_--; }

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
      add<Token>(TokType::EndFile);
      return false;
    }
    if (is_digit(at())) {
      return get_number();
    }
    if (is_alpha(at())) {
      return get_name();
    }
    error()<<"unknown token\n";
    return false;
  }

  bool get_name() {
    string name;
    for (; !done(); next()) {
      name.push_back(at());
    }
    add<PayloadToken<string>>(TokType::Name, name);
    return true;
  }

  bool get_number() {
    if (at() == '0') {
      next();
      if (!done()) {
        if (at() == 'x' || at() == 'X') {
          next();
          return hex_integer();
        } else if (at() == 'o' || at() == 'O') {
          next();
          return oct_integer();
        } else if (at() == 'b' || at() == 'B') {
          next();
          return bin_integer();
        }
      }
      back();
    }
    if (like_fp()) {
      return floating_point();
    }
    return dec_integer();
  }

  bool like_fp() {
    auto first = i_;
    auto fp = false;
    while (!done()) {
      if (is_digit(at())) {
        next();
      } else if (at() == '_') {
        next();
      } else if (at() == '.' || at() == 'e' || at() == 'E') {
        fp = true;
        break;
      } else {
        // Something else: maybe space, possibly i{N} suffix or invalid number.
        // Will detect later.
        break;
      }
    }
    i_ = first;
    return fp;
  }

  bool floating_point() {
    CHECK_FAIL("floating_point");
    return false;
  }

  bool dec_integer() {
    // 1234[i{8,16,32,64}]
    i64 n = 0;
    while (!done()) {
      if (is_digit(at())) {
        i64 d = at() - '0';
        // TODO: Detect overflow.
        n = 10 * n + d;
        next();
      } else if (at() == '_') {
        next();
        if (!is_digit(at())) {
          error()<<"digit has to follow separator _";
          return false;
        }
      } else if (at() == 'i' || at() == 'I') {
        CHECK_FAIL("i32 like suffix");
      } else if (is_alpha(at())) {
        error()<<"integer contains alpha character";
        return false;
      } else {
        break;
      }
    }
    add<LiteralToken<i64>>(TokType::Integer, Literal<i64>(LitType::Int, n));
    return true;
  }

  bool hex_integer() {
    CHECK_FAIL("hex_integer");
    return false;
  }
  bool oct_integer() {
    CHECK_FAIL("oct_integer");
    return false;
  }
  bool bin_integer() {
    CHECK_FAIL("bin_integer");
    return false;
  }

  template<class T, class ...ArgTs>
  void add(TokType type, ArgTs ...args) {
    tokens_->add(std::make_unique<T>(line_, i_ + 1, type, args...));
  }

  TokenStream* const tokens_ = nullptr;
  string s_;
  int line_ = 1;
  int i_ = 0;
  ErrStr &err_;
};
}

string to_string(LitType type) {
  switch (type) {
    case LitType::Int: return "Int";
    case LitType::I8: return "I8";
    case LitType::I16: return "I16";
    case LitType::I32: return "I32";
    case LitType::I64: return "I64";
    case LitType::F32: return "F32";
    case LitType::F64: return "F64";
    case LitType::Char: return "Char";
    case LitType::String: return "String";
  }
  return "";
}

string to_string(const Token& t) {
  std::stringstream ss;
  ss<<t;
  return ss.str();
}

std::ostream &Token::output(std::ostream &os) const {
  switch (type) {
    case TokType::EndFile:
      os<<"EndFile";
      break;
    case TokType::Integer: {
      auto t = get_payload<Literal<i64>>(*this);
      os<<"Integer("<<t.val<<": "<<to_string(t.type)<<")";
      break;
    }
    case TokType::Float: {
      auto t = get_payload<Literal<double>>(*this);
      os<<"Float("<<t.val<<": "<<to_string(t.type)<<")";
      break;
    }
    case TokType::String: {
      auto t = get_payload<string>(*this);
      os<<"String("<<t<<")";
      break;
    }
    case TokType::Char: {
      auto t = get_payload<i64>(*this);
      os<<"Char("<<t;
      if (' ' <= t && t < 127) {
        os<<"Char("<<char(t)<<")";
      } else {
        os<<"Char(#"<<t<<")";
      }
      break;
    }
    case TokType::Name: {
      auto t = get_payload<string>(*this);
      os<<"Name("<<t<<")";
      break;
    }
  }
  return os;
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
