#ifndef IGOR_LEXER_HXX
#define IGOR_LEXER_HXX

#include "base.hxx"

namespace igor {

enum class TokType : i64 {
  EndFile,
  Integer,
  Float,
  String,
  Char,
  Name,
  LParen,
  RParen,
  LCurly,
  RCurly,
  LBracket,
  RBracket,
  SemiColon,
  Comma,
  Colon,
  Period,
  Function,
  Module,
};

enum class LitType : i64 {
  Int,
  I8,
  I16,
  I32,
  I64,
  F32,
  F64,
  Char,
  String,
};

struct Token {
  Token() {}
  Token(int line, int col, TokType type): line(line), col(col), type(type) {}
  std::ostream &output(std::ostream &os) const;

  int line = 0;
  int col = 0;
  TokType type = TokType::EndFile;
};

template<class T>
struct Literal {
  Literal(LitType type, T val): type(type), val(val) {}
  const LitType type;
  const T val;

  bool operator==(const Literal &other) const {
    return type == other.type && val == other.val;
  }
  bool operator!=(const Literal &other) const {
    return !(*this == other);
  }
};

template<class T>
struct PayloadToken : public Token {
  PayloadToken(int line, int col, TokType type, T pl)
      : Token(line, col, type),
        pl(pl) {}

  const T pl;
};

template<class T>
using LiteralToken = PayloadToken<Literal<T>>;

template<class T>
T get_payload(const Token &t) {
  return reinterpret_cast<const PayloadToken<T>&>(t).pl;
}
template<class T>
Literal<T> get_literal(const Token &t) {
  return get_payload<Literal<T>>(t);
}

using TokenContainer = std::vector<std::unique_ptr<Token>>;

class TokenCursor {
public:
  TokenCursor(): tokens_(nullptr) {}
  explicit TokenCursor(const TokenContainer *tokens): tokens_(tokens) {}
  DEFAULT_COPY(TokenCursor);
  DEFAULT_MOVE(TokenCursor);
  Token *at() const { return (*tokens_)[i_].get(); }
  TokType type_at() const { return at()->type; }
  bool next() {
    if (i_ < tokens_->size()) {
      i_++;
      return true;
    }
    return false;
  }
  void back() { if (i_  > 0) i_--; }
  bool done() const { return i_ == tokens_->size(); }

private:
  TokenContainer const *tokens_ = nullptr;
  int i_ = 0;
};

class TokenStream {
public:
  explicit TokenStream(string file_name) : file_name_(std::move(file_name)) {}
  string file_name() const { return file_name_; }
  int size() const { return tokens_.size(); }
  void add(std::unique_ptr<Token> t);
  TokenCursor cursor() const { return TokenCursor(&tokens_); }

private:
  const std::string file_name_;
  TokenContainer tokens_;
};

class TokenErr {
public:
  TokenErr(string file, ErrStr &err)
      : file_(std::move(file)), err_(err) {}
  DEFAULT_COPY(TokenErr);
  DEFAULT_MOVE(TokenErr);

  std::stringstream &error(const TokenCursor &c) {
    return err_.error(file_, c.at()->line, c.at()->col);
  }
  bool ok() const { return err_.ok(); }
  void clear_error() { err_.clear_error(); }

private:
  const string file_;
  ErrStr &err_;
};

std::unique_ptr<TokenStream> tokenize(
    const std::string &file_name,
    std::string s,
    ErrStr &err);
bool check_name_at(TokenCursor c, TokenErr &err);
bool check_type_name_at(TokenCursor c, TokenErr &err);
string to_string(LitType type);

STREAM_OUT(const Token &t) {
  return t.output(os);
}
STREAM_OUT(LitType type) {
  return os<<to_string(type);
}
STREAM_OUT(TokenCursor cur) {
  return os<<*cur.at();
}

}

#endif
