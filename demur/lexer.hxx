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
  Function,
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

class TokenStream {
public:
  class Cursor {
  public:
    explicit Cursor(const TokenStream *tokens): tokens_(&tokens->tokens_) {}
    Token *at() const { return (*tokens_)[i_].get(); }
    void next() { if (i_ < tokens_->size()) i_++; }
    void back() { if (i_ > 0) i_--; }
    bool done() const { return i_ == tokens_->size(); }

  private:
    const std::vector<std::unique_ptr<Token>> *const tokens_ = nullptr;
    int i_ = 0;
  };

  explicit TokenStream(string file_name) : file_name_(std::move(file_name)) {}
  string file_name() const { return file_name_; }
  int size() const { return tokens_.size(); }
  void add(std::unique_ptr<Token> t);
  Cursor cursor() const { return Cursor(this); }

private:
  std::string file_name_;
  std::vector<std::unique_ptr<Token>> tokens_;
};

std::unique_ptr<TokenStream> tokenize(
    const std::string &file_name,
    std::string s,
    ErrStr &err);
string to_string(LitType type);
string to_string(const Token& t);

inline std::ostream &operator<<(std::ostream &os, const Token &t) {
  return t.output(os);
}
inline std::ostream &operator<<(std::ostream &os, LitType type) {
  return os<<to_string(type);
}

}

#endif
