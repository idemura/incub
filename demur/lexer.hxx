#ifndef IGOR_LEXER_HXX
#define IGOR_LEXER_HXX

#include "base.hxx"

namespace igor {

enum class TokType : i64 {
  EndFile,
  Literal,
  Function,
  Colon,
};

enum class LitType : i64 {
  Int,
  Int8,
  Int16,
  Int32,
  Int64,
  Float32,
  Float64,
  Char,
  String,
};

struct Token {
  Token() {}
  Token(int line, int col, TokType type): line(line), col(col), type(type) {}
  int line = 0;
  int col = 0;
  TokType type = TokType::EndFile;
};

template<class T>
struct TokenWithData : public Token {
  T data;
};

template<class T>
struct TokenLiteral {
  TokenLiteral(): val() {}
  LitType type = LitType::Int;
  const T val;
};

class TokenStream {
public:
  class Cursor {
  public:
    explicit Cursor(const TokenStream *tokens);
    void next();
    void back();
    bool eof();
    Token *get() const;

  private:
    const TokenStream *const tokens_ = nullptr;
    int ix_ = 0;
  };

  explicit TokenStream(string file_name) : file_name_(std::move(file_name)) {}
  string file_name() const { return file_name_; }
  int size() const { return tokens_.size(); }
  void add(std::unique_ptr<Token> t);
  Cursor get_cursor() const { return Cursor(this); }

private:
  std::string file_name_;
  std::vector<std::unique_ptr<Token>> tokens_;
};

std::unique_ptr<TokenStream> tokenize(
    const std::string &file_name,
    std::string s,
    ErrStr &err);

}

#endif
